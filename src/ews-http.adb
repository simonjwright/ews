--  This package is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or
--  (at your option) any later version.  It is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE.
--
--  As a special exception under Section 7 of GPL version 3, you are
--  granted additional permissions described in the GCC Runtime
--  Library Exception, version 3.1, as published by the Free Software
--  Foundation.
--
--  You should have received a copy of the GNU General Public License
--  and a copy of the GCC Runtime Library Exception along with this
--  program; see the files COPYING3 and COPYING.RUNTIME respectively.
--  If not, see <http://www.gnu.org/licenses/>.
--
--  Copyright (C) 2003-2022, Simon Wright <simon@pushface.org>
--  Copyright (C) 2022, Stephane Carrez <Stephane.Carrez@gmail.com>

pragma Ada_2012;

with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Strings.Bounded;
with Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with GNAT.Regpat;
with Interfaces.C;

with EWS.Dynamic;
with EWS.Static;

package body EWS.HTTP is

   package Str is new Ada.Strings.Bounded.Generic_Bounded_Length (1024);


   use GNAT.Sockets;
   package SS renames Smart_Strings;

   --  RFC 3986 Uniform Resource Identifier (URI): Generic Syntax,
   --  Appendix B.  Parsing a URI Reference with a Regular Expression
   --  defines the following regular expression:
   --  ^(([^:/?#]+):)?(//([^/?#]*))?([^?#]*)(\?([^#]*))?(#(.*))?
   URL_Request : constant String :=
     "(\r\n)?"
     & "(GET|POST|HEAD|PUT|DELETE|OPTIONS|PATCH)" -- request, 2
     & "\s"
     & "(/|((/[a-z0-9._-]+)+)/?)"                 -- the URL, 3
     & "(\?([^#]*))?"                             -- query, 6
     & "\s"
     & "HTTP/(\d\.\d)"                            -- version, 8
     & "\r\n";

   Method_Match : constant := 2;
   URL_Match     : constant := 3;
   Query_Match   : constant := 6;
   Version_Match : constant := 8;

   URL_Matcher : constant GNAT.Regpat.Pattern_Matcher :=
     GNAT.Regpat.Compile (URL_Request,
                          Flags => GNAT.Regpat.Case_Insensitive);

   URL_Max_Parens : constant GNAT.Regpat.Match_Count :=
     GNAT.Regpat.Paren_Count (URL_Matcher);

   CRLF : constant String := CR & LF;


   ---------------------
   --  Utility specs  --
   ---------------------

   procedure Determine_Line_Style (Used_In : in out Cursor);

   procedure Free_Stream
   is new Ada.Unchecked_Deallocation (Ada.Streams.Root_Stream_Type'Class,
                                      Stream_Access);

   function Get_Content_Length (From : String) return Natural;

   function Index
     (Source  : String;
      Pattern : String;
      From    : Positive;
      Going   : Ada.Strings.Direction              := Ada.Strings.Forward;
      Mapping : Ada.Strings.Maps.Character_Mapping
        := Ada.Strings.Maps.Identity)
     return Natural;
   --  A replacement for Ada05 Ada.Strings.Fixed.Index.

   function Plus_To_Space (S : String) return String;

   function Read_Request (From : Socket_Type) return String;

   function To_String
     (In_String : String;
      From      : GNAT.Regpat.Match_Location) return String
     with Inline;

   function Unescape (S : String) return String;


   -------------------------
   --  Public operations  --
   -------------------------

   procedure Initialize (R          : out Request;
                         From       :     GNAT.Sockets.Socket_Type;
                         Terminated : out Boolean)
   is
      S : Stream_Access := Stream (From);
   begin
      R.Head := SS.Create (new String'(Read_Request (From)));
      declare
         Content_Length : constant Natural
           := Get_Content_Length (SS.Value (R.Head).all);
      begin
         if Content_Length > 0 then
            R.Content := SS.Create (new String (1 .. Content_Length));
            String'Read (S, SS.Value (R.Content).all);
         end if;
      end;
      Free_Stream (S);
      Terminated := SS.Value (R.Head)'Length = 0;
   exception
      when GNAT.Sockets.Socket_Error =>
         --  This is what happens on VxWorks when the peer closes the
         --  socket; other OSs happily read an empty header.
         Terminated := True;
   end Initialize;


   function Get_Method (From : Request) return Method
   is
      Matches : GNAT.Regpat.Match_Array (0 .. URL_Max_Parens);
      Input : String renames SS.Value (From.Head).all;
      use type GNAT.Regpat.Match_Location;
   begin
      GNAT.Regpat.Match (URL_Matcher, Input, Matches);
      if Matches (0) = GNAT.Regpat.No_Match then
         return "";
      else
         return To_String (Input, Matches (Method_Match));
      end if;
   end Get_Method;


   function Get_Version (From : Request) return Version
   is
      Matches : GNAT.Regpat.Match_Array (0 .. URL_Max_Parens);
      Input : String renames SS.Value (From.Head).all;
      use type GNAT.Regpat.Match_Location;
   begin
      GNAT.Regpat.Match (URL_Matcher, Input, Matches);
      if Matches (0) = GNAT.Regpat.No_Match then
         return "";
      else
         return To_String (Input, Matches (Version_Match));
      end if;
   end Get_Version;


   function Get_URL (From : Request) return URL
   is
      Matches : GNAT.Regpat.Match_Array (0 .. URL_Max_Parens);
      Input : String renames SS.Value (From.Head).all;
      use type GNAT.Regpat.Match_Location;
   begin
      GNAT.Regpat.Match (URL_Matcher, Input, Matches);
      if Matches (0) = GNAT.Regpat.No_Match then
         return "";
      else
         return Unescape (To_String (Input, Matches (URL_Match)));
      end if;
   end Get_URL;


   function Get_Property (Named : String;
                          From  : Request) return Property
   is
      Query_Matches : GNAT.Regpat.Match_Array (0 .. URL_Max_Parens);
      Query_Input : String renames SS.Value (From.Head).all;
      Property_Matcher : constant GNAT.Regpat.Pattern_Matcher :=
        GNAT.Regpat.Compile ("(^|&|\?)" & Named & "=([^&]*)",
                             Flags => GNAT.Regpat.Case_Insensitive);
      Property_Matches : GNAT.Regpat.Match_Array (0 .. 2);
      use type GNAT.Regpat.Match_Location;
   begin
      GNAT.Regpat.Match (URL_Matcher, Query_Input, Query_Matches);
      --  which has to succeed, we wouldn't get here with an illegal head
      pragma Assert (Query_Matches (0) /= GNAT.Regpat.No_Match);
      if Ada.Strings.Fixed.Translate
        (To_String (Query_Input, Query_Matches (Method_Match)),
         Ada.Strings.Maps.Constants.Upper_Case_Map)
        in "GET" | "OPTIONS" | "HEAD"
        and then Query_Matches (Query_Match) /= GNAT.Regpat.No_Match
      then
         declare
            Property_Input : constant String :=
              To_String (Query_Input, Query_Matches (Query_Match));
         begin
            GNAT.Regpat.Match
              (Property_Matcher, Property_Input, Property_Matches);
            if Property_Matches (0) = GNAT.Regpat.No_Match then
               return "";
            else
               return Plus_To_Space
                 (Unescape (To_String (Property_Input, Property_Matches (2))));
            end if;
         end;
      elsif Ada.Strings.Fixed.Translate
        (To_String (Query_Input, Query_Matches (Method_Match)),
         Ada.Strings.Maps.Constants.Upper_Case_Map)
        in "POST" | "PUT" | "DELETE" | "PATCH"
        and then SS.Value (From.Content) /= null
      then
         declare
            Property_Input : String renames SS.Value (From.Content).all;
         begin
            GNAT.Regpat.Match
              (Property_Matcher, Property_Input, Property_Matches);
            if Property_Matches (0) = GNAT.Regpat.No_Match then
               return "";
            else
               return Plus_To_Space
                 (Unescape (To_String (Property_Input, Property_Matches (2))));
            end if;
         end;
      else
         return "";
      end if;
   end Get_Property;


   function Get_Field (Named : String; From : Request) return Property
   is
      Field_Request : constant String
        := Named & ":\s*([[:^cntrl:]]+(\r\n\s[[:^cntrl:]]+)*)";
      Field_Matcher : constant GNAT.Regpat.Pattern_Matcher :=
        GNAT.Regpat.Compile (Field_Request,
                             Flags => GNAT.Regpat.Case_Insensitive);
      Field_Max_Parens : constant GNAT.Regpat.Match_Count :=
        GNAT.Regpat.Paren_Count (Field_Matcher);
      Matches : GNAT.Regpat.Match_Array (0 .. Field_Max_Parens);
      use type GNAT.Regpat.Match_Location;
   begin
      GNAT.Regpat.Match (Field_Matcher, SS.Value (From.Head).all, Matches);
      if Matches (0) = GNAT.Regpat.No_Match then
         return "";
      else
         return To_String (SS.Value (From.Head).all, Matches (1));
      end if;
   end Get_Field;


   function Keep_Alive_After_Responding (The_Request : Request) return Boolean
   is
   begin
      if Get_Version (The_Request) = "1.0" then
         return Get_Field ("Connection", From => The_Request) = "Keep-Alive";
      else
         return Get_Field ("Connection", From => The_Request) /= "close";
      end if;
   end Keep_Alive_After_Responding;


   --  Debug support

   function Get_Head (From : Request) return String
   is
   begin
      return SS.Value (From.Head).all;
   end Get_Head;


   function Get_Body (From : Request) return String
   is
   begin
      return SS.Value (From.Content).all;
   end Get_Body;


   --  Content/attachment management  --

   function Get_Attachments (From : Request) return Attachments
   is
   begin
      return Attachments (From);
   end Get_Attachments;


   procedure Clear (The_Attachments : in out Attachments)
   is
   begin
      The_Attachments := (Head => SS.Null_Pointer, Content => SS.Null_Pointer);
   end Clear;


   function Get_Field (Named : String;
                       From  : Attachments;
                       Index : Positive    := 1) return Property
   is
   begin
      if SS.Value (From.Content) = null then
         return "";
      else
         declare
            Part_Start : Positive;
            Part_Finish : Natural;
         begin
            Locate_Whole_Body_Part (From, Index, Part_Start, Part_Finish);
            declare
               Whole_Part : String
                 renames SS.Value (From.Content) (Part_Start .. Part_Finish);
               Finish : constant Natural :=
                 HTTP.Index (Whole_Part, CRLF & CRLF);
               Headers : String
                 renames Whole_Part (Whole_Part'First .. Finish);
               Field_Request : constant String
                 := Named & ":\s*([[:^cntrl:]]+(\r\n\s[[:^cntrl:]]+)*)";
               Field_Matcher : constant GNAT.Regpat.Pattern_Matcher :=
                 GNAT.Regpat.Compile (Field_Request,
                                      Flags => GNAT.Regpat.Case_Insensitive);
               Field_Max_Parens : constant GNAT.Regpat.Match_Count :=
                 GNAT.Regpat.Paren_Count (Field_Matcher);
               Matches : GNAT.Regpat.Match_Array (0 .. Field_Max_Parens);
               use type GNAT.Regpat.Match_Location;
            begin
               GNAT.Regpat.Match (Field_Matcher, Headers, Matches);
               if Matches (0) = GNAT.Regpat.No_Match then
                  return "";
               else
                  return To_String (Headers, Matches (1));
               end if;
            end;
         end;
      end if;
   end Get_Field;


   --  String content

   Empty_String : aliased constant String := "";

   function Get_Content (From  : Attachments;
                         Index : Positive    := 1) return Contents
   is
   begin
      if SS.Value (From.Content) = null then
         return Empty_String'Access;
      else
         declare
            Part_Start : Positive;
            Part_Finish : Natural;
         begin
            Locate_Whole_Body_Part (From, Index, Part_Start, Part_Finish);
            declare
               Whole_Part : String
                 renames SS.Value (From.Content) (Part_Start .. Part_Finish);
               Start : constant Natural :=
                 HTTP.Index (Whole_Part, CRLF & CRLF);
            begin
               --  Strip the header fields (if any) & the CRLF delimiter
               --  pair.
               return new String'(Whole_Part (Start + 4 .. Whole_Part'Last));
            end;
         end;
      end if;
   end Get_Content;


   function Get_Content_Kind (From  : Contents) return Content_Kind
   is
     (if (for some C of From.all => Character'Pos (C) > 127) then
         Binary
      else
         Text);


   --  Text content

   procedure Open (C     : in out Cursor;
                   From  :        Attachments;
                   Index :        Positive    := 1)
   is
   begin
      if C.Open then
         raise Status_Error;
      end if;
      C.Open := True;
      C.Line_Ending := Unknown;
      C.Data := Get_Content (From, Index);
      C.Start := C.Data'First;
      C.Last := C.Data'Last;
      C.Next := C.Start;
      Determine_Line_Style (C);
   end Open;


   procedure Open (C     : in out Cursor;
                   From  :        Contents)
   is
   begin
      if C.Open then
         raise Status_Error;
      end if;
      C.Open := True;
      C.Line_Ending := Unknown;
      C.Data := From;
      C.Start := C.Data'First;
      C.Last := C.Data'Last;
      C.Next := C.Start;
      Determine_Line_Style (C);
   end Open;


   procedure Close (C : in out Cursor)
   is
   begin
      if not C.Open then
         raise Status_Error;
      end if;
      C.Open := False;
      --  XXXXXXXXXXXXXXXXXXXXXXXX Clear (C.Data);
   end Close;


   function End_Of_File (C : Cursor) return Boolean
   is
   begin
      if not C.Open then
         raise Status_Error;
      end if;
      return C.Next > C.Last;
   end End_Of_File;


   procedure Get_Line (C    : in out Cursor;
                       Line :    out String;
                       Last :    out Natural)
   is
   begin
      if not C.Open then
         raise Status_Error;
      end if;
      if C.Next > C.Last then
         raise End_Error;
      end if;
      declare
         Text : String renames C.Data.all;
         CR : constant String := (1 => ASCII.CR);
         LF : constant String := (1 => ASCII.LF);
         Terminator : Natural;
      begin
         case C.Line_Ending is
            when Unknown =>
               raise Program_Error;
            when Unterminated =>
               Terminator := 0;
            when Unix =>
               Terminator := Index (Text, LF, C.Next);
            when Windows =>
               Terminator := Index (Text, CR, C.Next);
         end case;
         if Terminator = 0 then
            --  NB, this covers the case when there's no line ending
            --  left. The whole of the rest of the string is
            --  available
            Terminator := C.Last + 1;
         end if;
         declare
            Source_Length : constant Natural := Terminator - C.Next;
            Target_Length : constant Natural := Line'Length;
         begin
            if Source_Length = 0 then
               Last := Line'First - 1;
            elsif Source_Length <= Target_Length then
               Last := Line'First + Source_Length - 1;
               Line (Line'First .. Line'First + Source_Length - 1)
                 := Text (C.Next .. C.Next + Source_Length - 1);
               C.Next := C.Next + Source_Length;
            else
               Last := Line'Last;
               Line := Text (C.Next .. C.Next + Target_Length - 1);
               C.Next := C.Next + Target_Length;
            end if;
         end;
         --  skip pending LF or CRLF
         if C.Next <= C.Last then
            if C.Line_Ending = Unix and then Text (C.Next) = LF (1) then
               C.Next := C.Next + 1;
            elsif C.Line_Ending = Windows and then Text (C.Next) = CR (1) then
               C.Next := C.Next + 2;
            end if;
         end if;
      end;
   end Get_Line;


   ---------------------------
   --  Response management  --
   ---------------------------

   function Find (For_Request : not null access Request) return Response'Class
   is
   begin
      declare
         R : constant Response'Class
           := Dynamic.Find (For_Request);
      begin
         if R in Dynamic.Dynamic_Response'Class then
            return R;
         end if;
      end;
      return Static.Find (For_Request);
   exception
      when E : others =>
         Put_Line ("failed in read/respond, "
                     & Ada.Exceptions.Exception_Information (E));
         return Exception_Response (E, For_Request);
   end Find;


   -------------------------------
   --  Default implementations  --
   -------------------------------

   function Response_Kind (This : Response) return String
   is
      pragma Unreferenced (This);
   begin
      return "200 OK";
   end Response_Kind;


   function Cacheable (This : Response) return Boolean
   is
      pragma Unreferenced (This);
   begin
      return True;
   end Cacheable;


   function Content_Type (This : Response) return String
   is
      pragma Unreferenced (This);
   begin
      return "text/plain";
   end Content_Type;


   function Content_Length (This : Response) return Integer
   is
   begin
      return Content (Response'Class (This))'Length;
      --  NB the dispatching call.
   end Content_Length;


   function Content (This : Response) return String
   is
      pragma Unreferenced (This);
   begin
      return "";
   end Content;


   function Headers (This : Response) return String
   is
   begin
      return "Content-Type: " & Content_Type (Response'Class (This)) & CRLF &
        "Content-Length: " & Content_Length (Response'Class (This))'Img & CRLF;
      --  NB the dispatching call.
   end Headers;


   procedure Write_Content
     (This :                 Response;
      To   : not null access Ada.Streams.Root_Stream_Type'Class)
   is
   begin
      String'Write (To, Content (Response'Class (This)));
      --  NB the dispatching call.
   end Write_Content;


   procedure Respond (This : Response'Class;
                      To   : GNAT.Sockets.Socket_Type)
   is
      U : aliased Unbounded_Memory_Stream;
   begin
      if Get_Version (This.To.all) = "1.0" then
         String'Write
           (U'Access,
            "HTTP/1.0 " & Response_Kind (This) & CRLF &
              "Server: EWS" & CRLF &
              Headers (This));
         if Keep_Alive_After_Responding (This.To.all) then
            String'Write (U'Access, "Connection: Keep-Alive" & CRLF);
         end if;
      else
         String'Write
           (U'Access,
            "HTTP/1.1 " & Response_Kind (This) & CRLF &
              "Server: EWS" & CRLF &
              Headers (This));
         if not Keep_Alive_After_Responding (This.To.all) then
            String'Write (U'Access, "Connection: close" & CRLF);
         end if;
      end if;
      if not Cacheable (This) then
         String'Write (U'Access, "Cache-Control: no-cache" & CRLF);
      end if;
      String'Write (U'Access, CRLF);
      Write_Content (This, U'Access);
      Copy (U, To);
   end Respond;


   ------------------------------------------------------
   --  Simple (error) responses and factory functions  --
   ------------------------------------------------------

   type Not_Found_Response (To : Request_P)
   is new Response (To) with null record;

   overriding
   function Response_Kind (This : Not_Found_Response) return String;
   overriding
   function Content (This : Not_Found_Response) return String;

   function Not_Found
     (R : not null access Request) return Response'Class is
   begin
      return Not_Found_Response'(To => Request_P (R));
   end Not_Found;


   type Not_Implemented_Response (To : Request_P)
   is new Response (To) with null record;

   overriding
   function Response_Kind (This : Not_Implemented_Response) return String;
   overriding
   function Content (This : Not_Implemented_Response) return String;

   function Not_Implemented
     (R : not null access Request) return Response'Class is
   begin
      return Not_Implemented_Response'(To => Request_P (R));
   end Not_Implemented;


   type Exception_Response_T (To : Request_P)
   is new Response (To) with record
         Info : Str.Bounded_String;
   end record;

   overriding
   function Response_Kind (This : Exception_Response_T) return String;
   overriding
   function Content (This : Exception_Response_T) return String;

   function Exception_Response
     (E : Ada.Exceptions.Exception_Occurrence;
      R : access Request) return Response'Class
   is
      Info : constant String := Ada.Exceptions.Exception_Information (E);
   begin
      if Info'Length > Str.Max_Length then
         return Exception_Response_T'
           (To => Request_P (R),
            Info =>
              Str.To_Bounded_String
              (Info (Info'First .. Info'First + Str.Max_Length - 1)));
      else
         return Exception_Response_T'
           (To => Request_P (R),
            Info => Str.To_Bounded_String (Info));
      end if;
   end Exception_Response;


   ----------------------
   --  Utility bodies  --
   ----------------------

   procedure Determine_Line_Style (Used_In : in out Cursor)
   is
      Text : String renames Used_In.Data.all;
      CR : constant String := (1 => ASCII.CR);
      LF : constant String := (1 => ASCII.LF);
   begin
      if Used_In.Line_Ending = Unknown then
         declare
            CR_Index : constant Natural := Index (Text, CR, Used_In.Next);
            LF_Index : constant Natural := Index (Text, LF, Used_In.Next);
         begin
            if CR_Index = 0 and LF_Index = 0 then
               Used_In.Line_Ending := Unterminated;
            elsif CR_Index = 0 then
               Used_In.Line_Ending := Unix;
            elsif LF_Index = 0 then
               Used_In.Line_Ending := Windows;
            elsif CR_Index < LF_Index then
               Used_In.Line_Ending := Windows;
            else
               Used_In.Line_Ending := Unix;
            end if;
         end;
      end if;
   end Determine_Line_Style;


   function Get_Content_Length (From : String) return Natural
   is
      Content_Length_Request : constant String :=
        "Content-Length:\s([0-9]+)\r\n";
      Content_Length_Matcher : constant GNAT.Regpat.Pattern_Matcher :=
        GNAT.Regpat.Compile (Content_Length_Request,
                             Flags => GNAT.Regpat.Case_Insensitive);
      Content_Length_Max_Parens : constant GNAT.Regpat.Match_Count :=
        GNAT.Regpat.Paren_Count (Content_Length_Matcher);
      Matches : GNAT.Regpat.Match_Array (0 .. Content_Length_Max_Parens);
      use type GNAT.Regpat.Match_Location;
   begin
      GNAT.Regpat.Match (Content_Length_Matcher, From, Matches);
      if Matches (0) = GNAT.Regpat.No_Match then
         return 0;
      else
         return Natural'Value (To_String (From, Matches (1)));
      end if;
   end Get_Content_Length;


   --  This is a reworking of the GNAT-GPL-2006
   --  Ada.Strings.Search.Index which doesn't copy the whole Source
   --  onto the stack.
   function Index
     (Source  : String;
      Pattern : String;
      Going   : Ada.Strings.Direction := Ada.Strings.Forward;
      Mapping : Ada.Strings.Maps.Character_Mapping
        := Ada.Strings.Maps.Identity)
     return Natural
   is
      Cur_Index       : Natural;
      Potential_Match : Boolean;
      use Ada.Strings;
      use Ada.Strings.Maps;
   begin
      if Pattern = "" then
         raise Pattern_Error;
      end if;

      --  Forwards case

      if Going = Forward then
         for J in 1 .. Source'Length - Pattern'Length + 1 loop
            Cur_Index := Source'First + J - 1;
            Potential_Match := True;
            for K in Pattern'Range loop
               if Pattern (K) /=
                 Value (Mapping, Source (Cur_Index + K - 1))
               then
                  Potential_Match := False;
                  exit;
               end if;
            end loop;
            if Potential_Match then
               return Cur_Index;
            end if;
         end loop;

      --  Backwards case

      else
         for J in reverse 1 .. Source'Length - Pattern'Length + 1 loop
            Cur_Index := Source'First + J - 1;
            Potential_Match := True;
            for K in Pattern'Range loop
               if Pattern (K) /=
                 Value (Mapping, Source (Cur_Index + K - 1))
               then
                  Potential_Match := False;
                  exit;
               end if;
            end loop;
            if Potential_Match then
               return Cur_Index;
            end if;
         end loop;
      end if;

      --  Fall through if no match found. Note that the loops are skipped
      --  completely in the case of the pattern being longer than the source.

      return 0;
   end Index;


   function Index
     (Source : String;
      Pattern : String;
      From : Positive;
      Going : Ada.Strings.Direction := Ada.Strings.Forward;
      Mapping : Ada.Strings.Maps.Character_Mapping
        := Ada.Strings.Maps.Identity)
     return Natural
   is
      Candidate : String renames Source (From .. Source'Last);
   begin
      return Index (Source => Candidate,
                    Pattern => Pattern,
                    Going => Going,
                    Mapping => Mapping);
   end Index;


   procedure Locate_Whole_Body_Part (Within :     Attachments;
                                     Index  :     Positive    := 1;
                                     Start  : out Positive;
                                     Finish : out Natural)
   is
      Content_Type : constant String := Get_Field ("Content-Type",
                                                   From => Request (Within));
      Text : String_P renames SS.Value (Within.Content);
   begin
      if Text = null then
         Start := 1;
         Finish := 0;
         return;
      elsif Content_Type'Length = 0
        or else HTTP.Index (Content_Type, "multipart") = 0
      then
         Start := Text'First;
         Finish := Text'Last;
         return;
      end if;
      declare
         Marker : constant String := "boundary=";
         Boundary : constant String :=
           "--" &
           Content_Type (HTTP.Index (Content_Type, Marker)
                           + Marker'Length .. Content_Type'Last);
         Part : Natural := 0;
      begin
         Start := Text'First + Boundary'Length + 1; -- past the CRLF
         loop
            if Start > Text'Last - Boundary'Length then
               --  Problem with Index on GNAT-GPL-2006; would have
               --  expected 0 even on the trailing boundary (should
               --  have trailing --) but got Constraint_Error.
               raise Name_Error;
            end if;
            Finish := HTTP.Index (Text.all, Boundary, Start);
            if Finish < Start then
               raise Name_Error;
            end if;
            Part := Part + 1;
            if Part = Index then
               --  Omit the trailing CRLF.
               Finish := Finish - 3;
               exit;
            end if;
            --  Not done yet, on to the next part.
            Start := Finish + Boundary'Length + 1;
         end loop;
      end;
   end Locate_Whole_Body_Part;


   function Plus_To_Space (S : String) return String
   is
      Mapping : constant Ada.Strings.Maps.Character_Mapping
        := Ada.Strings.Maps.To_Mapping (From => "+",
                                        To => " ");
   begin
      return Ada.Strings.Fixed.Translate (S, Mapping);
   end Plus_To_Space;


   function Read_Request (From : Socket_Type) return String
   is
      use type Ada.Streams.Stream_Element_Array;
      use type Ada.Streams.Stream_Element_Offset;
      Tmp : Ada.Streams.Stream_Element_Array (1 .. 2048);
      Last : Ada.Streams.Stream_Element_Offset := Tmp'First - 1;
      Next : Ada.Streams.Stream_Element_Offset;
      Termination : constant Ada.Streams.Stream_Element_Array :=
        (Character'Pos (CR),
         Character'Pos (LF),
         Character'Pos (CR),
         Character'Pos (LF));
      S : Stream_Access := Stream (From);
   begin

      --  We need to read the whole request from the client. Of course
      --  we don't know how long it is. We can't just issue an
      --  Ada.Streams.Read for a large buffer, because the client may
      --  not have sent that much and if she hasn't we'll block until
      --  she gives up and closes the socket. So we read a character
      --  at a time until we've got the CR/LF/CR/LF which terminates
      --  the line.
      loop
         Ada.Streams.Read (Stream => S.all,
                           Item => Tmp (Last + 1 .. Last + 1),
                           Last => Next);
         exit when Next = Last;
         Last := Last + 1;
         exit when Last >= Termination'Length
           and then Tmp (Last - 3 .. Last) = Termination;
         exit when Last = Tmp'Last;
      end loop;

      Free_Stream (S);

      declare
         Result : String (1 .. Natural (Last)) with Import, Convention => Ada;
         for Result'Address use Tmp'Address;
      begin
         return Result;
      end;

   end Read_Request;


   function To_String
     (In_String : String;
      From      : GNAT.Regpat.Match_Location) return String
   is
      Last : Natural := From.Last;
   begin
      --  Konqueror has been known to append a \0
      while Last >= From.First and then In_String (Last) = ASCII.NUL loop
         Last := Last - 1;
      end loop;
      return In_String (From.First .. Last);
   end To_String;


   function Unescape (S : String) return String
   is

      function Hex (H : String) return Natural;
      function Hex (H : String) return Natural
      is
         Result : Natural := 0;
      begin
         for I in H'Range loop
            declare
               C : constant Character := H (I);
               D : Natural;
            begin
               case C is
                  when '0' .. '9' =>
                     D := Character'Pos (C) - Character'Pos ('0');
                  when 'a' .. 'f' =>
                     D := Character'Pos (C) - Character'Pos ('a') + 16#A#;
                  when 'A' .. 'F' =>
                     D := Character'Pos (C) - Character'Pos ('A') + 16#A#;
                  when others =>
                     raise Constraint_Error;
               end case;
               Result := Result * 16#10# + D;
            end;
         end loop;
         return Result;
      end Hex;

      Result : String (S'Range);
      Next_In : Positive := S'First;
      Next_Out : Positive := Next_In;

   begin
      if S'Length = 0 then
         return S;
      else
         loop
            if S (Next_In) /= '%' then
               Result (Next_Out) := S (Next_In);
               Next_In := Next_In + 1;
               Next_Out := Next_Out + 1;
            else
               Result (Next_Out) :=
                 Character'Val (Hex (S (Next_In + 1 .. Next_In + 2)));
               Next_In := Next_In + 3;
               Next_Out := Next_Out + 1;
            end if;
            exit when Next_In > S'Last;
         end loop;
         return Result (Result'First .. Next_Out - 1);
      end if;
   end Unescape;


   -----------------------------
   --  Error response bodies  --
   -----------------------------

   function Response_Kind (This : Not_Found_Response) return String
   is
      pragma Unreferenced (This);
   begin
      return "404 Not Found";
   end Response_Kind;

   function Content (This : Not_Found_Response) return String
   is
      pragma Unreferenced (This);
   begin
      return "Not found.";
   end Content;


   function Response_Kind (This : Not_Implemented_Response) return String
   is
      pragma Unreferenced (This);
   begin
      return "501 Not implemented";
   end Response_Kind;

   function Content (This : Not_Implemented_Response) return String
   is
      pragma Unreferenced (This);
   begin
      return "Not implemented.";
   end Content;


   function Response_Kind (This : Exception_Response_T) return String
   is
      pragma Unreferenced (This);
   begin
      return "500 Internal server error";
   end Response_Kind;

   function Content (This : Exception_Response_T) return String is
   begin
      return "Exception: " & Str.To_String (This.Info);
   end Content;


   ---------------------------------------
   --  Unbounded Memory Streams bodies  --
   ---------------------------------------


   procedure Free
   is new Ada.Unchecked_Deallocation (Stream_Chunk, Stream_Chunk_P);


   procedure Finalize (UMSF : in out Unbounded_Memory_Stream_Finalizer)
   is
      Current : Stream_Chunk_P := UMSF.UMS.Head;
   begin
      while Current /= null loop
         declare
            Next : constant Stream_Chunk_P := Current.Next;
         begin
            Free (Current);
            Current := Next;
         end;
      end loop;
   end Finalize;


   procedure Copy  (Stream : Unbounded_Memory_Stream;
                    To     : GNAT.Sockets.Socket_Type)
   is
      Chunks : Natural := 0;
   begin
      declare
         Chunk : Stream_Chunk_P := Stream.Head;
      begin
         while Chunk /= null loop
            Chunks := Chunks + 1;
            Chunk := Chunk.Next;
         end loop;
      end;
      declare
         use Ada.Streams;
         Vector : GNAT.Sockets.Vector_Type (1 .. Chunks);
         Chunk : Stream_Chunk_P := Stream.Head;
         Index : Positive := Vector'First;
         Bytes_To_Send : Stream_Element_Count := Stream.Length;
         Bytes_Sent : Stream_Element_Count;
      begin
         while Chunk /= null loop
            Vector (Index) :=
              (Base => Chunk.Elements (Chunk.Elements'First)'Access,
               Length => Interfaces.C.size_t
                 (Stream_Element_Offset'Min
                    (Bytes_To_Send, Stream_Chunk_Elements'Length)));
            Bytes_To_Send :=
              Bytes_To_Send - Stream_Element_Offset (Vector (Index).Length);
            Chunk := Chunk.Next;
            Index := Index + 1;
         end loop;
         GNAT.Sockets.Send_Vector (Socket => To,
                                   Vector => Vector,
                                   Count  => Bytes_Sent);
         pragma Assert (Bytes_Sent = Stream.Length,
                        "byte count mismatch in Copy");
      end;
   end Copy;


   --  Read isn't meant to be called; output contents via Copy.
   procedure Read  (Stream : in out Unbounded_Memory_Stream;
                    Item   :    out Ada.Streams.Stream_Element_Array;
                    Last   :    out Ada.Streams.Stream_Element_Offset)
   is
   begin
      raise Program_Error;
   end Read;


   procedure Write (Stream : in out Unbounded_Memory_Stream;
                    Item   :        Ada.Streams.Stream_Element_Array)
   is
      use Ada.Streams;
      First_Byte_In_Item : Stream_Element_Offset := Item'First;
      Bytes_To_Write : Stream_Element_Offset := Item'Length;
   begin
      loop
         exit when Bytes_To_Write = 0;
         declare
            Bytes_Remaining_In_Chunk : constant Stream_Element_Count :=
              Stream_Chunk_Elements'Length
              - Stream.Length mod Stream_Chunk_Elements'Length;
            --  NB! If Stream.Length (the number of bytes written so
            --  far) is a multiple of Stream_Chunk_Elements'Length,
            --  there will be no space left in the Tail chunk (if
            --  there is one, there won't be at the first Write) BUT
            --  the Bytes_Remaining_In_Chunk will be
            --  Stream_Chunk_Elements'Length. In any other case, the
            --  count will be correct.
         begin
            if Bytes_Remaining_In_Chunk = Stream_Chunk_Elements'Length then
               --  The Tail chunk, if any, is full; allocate another.
               declare
                  New_Chunk : constant Stream_Chunk_P := new Stream_Chunk;
               begin
                  if Stream.Head = null then
                     --  If this is the first Write, both Head and
                     --  Tail will be null;
                     Stream.Head := New_Chunk;
                     Stream.Tail := New_Chunk;
                  else
                     --  otherwise, tack the new chunk on at Tail.
                     Stream.Tail.Next := New_Chunk;
                     Stream.Tail := New_Chunk;
                  end if;
               end;
            end if;
            declare
               First_Byte_In_Chunk : constant Stream_Element_Count
                 := Stream_Chunk_Elements'Last - Bytes_Remaining_In_Chunk + 1;
               Bytes_To_Write_Now : constant Stream_Element_Count
                 := Stream_Element_Count'Min (Bytes_To_Write,
                                              Bytes_Remaining_In_Chunk);
            begin
               Stream.Tail.Elements
                 (First_Byte_In_Chunk ..
                    First_Byte_In_Chunk + Bytes_To_Write_Now - 1)
                 := Item
                 (First_Byte_In_Item ..
                    First_Byte_In_Item + Bytes_To_Write_Now - 1);
               Bytes_To_Write := Bytes_To_Write - Bytes_To_Write_Now;
               Stream.Length := Stream.Length + Bytes_To_Write_Now;
               First_Byte_In_Item := First_Byte_In_Item + Bytes_To_Write_Now;
            end;
         end;
      end loop;
   end Write;


end EWS.HTTP;
