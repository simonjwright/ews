--  Copyright (C) Simon Wright <simon@pushface.org>

--  This package is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2, or
--  (at your option) any later version. This package is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE. See the GNU General Public License for more
--  details. You should have received a copy of the GNU General Public
--  License distributed with this package; see file COPYING.  If not,
--  write to the Free Software Foundation, 59 Temple Place - Suite
--  330, Boston, MA 02111-1307, USA.

--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this unit does not by itself cause the resulting
--  executable to be covered by the GNU General Public License.  This
--  exception does not however invalidate any other reasons why the
--  executable file might be covered by the GNU Public License.

--  $RCSfile$
--  $Revision$
--  $Date$
--  $Author$

with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Strings.Bounded;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with GNAT.Regpat;

with EWS.Dynamic;
with EWS.Static;

package body EWS.HTTP is


   package Str is new Ada.Strings.Bounded.Generic_Bounded_Length (1024);


   use GNAT.Sockets;
   use Smart_Strings;


   URL_Request : constant String :=
     "(\r\n)?"
     & "(GET|POST)"                             -- request, 2
     & "\s"
     & "(/|((/[a-z0-9._-]+)+)/?)"               -- the URL, 3
     & "(\?([a-z0-9._=&%-]+))?"                 -- query, 6
     & "\s"
     & "HTTP/(\d\.\d)"                          -- version, 8
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

   function To_String
     (In_String : String;
      From : GNAT.Regpat.Match_Location) return String;
   pragma Inline (To_String);

   function Unescape (S : String) return String;

   function Plus_To_Space (S : String) return String;

   function Read_Request (From : Socket_Type) return String;

   function Get_Content_Length (From : String) return Natural;

   --  Find the bounds in the Contents of Within of the Index'th
   --  part. The bounds include any leading properties.
   procedure Locate_Whole_Body_Part (Within : Attachments;
                                     Index : Positive := 1;
                                     Start : out Positive;
                                     Finish : out Natural);

   procedure Free_Stream
   is new Ada.Unchecked_Deallocation (Ada.Streams.Root_Stream_Type'Class,
                                      Stream_Access);


   -------------------------
   --  Public operations  --
   -------------------------

   procedure Initialize (R : out Request;
                         From : GNAT.Sockets.Socket_Type;
                         Terminated : out Boolean) is
      S : Stream_Access := Stream (From);
   begin
      R.Head := Create (new String'(Read_Request (From)));
      declare
         Content_Length : constant Natural
           := Get_Content_Length (Value (R.Head).all);
      begin
         if Content_Length > 0 then
            R.Content := Create (new String (1 .. Content_Length));
            String'Read (S, Value (R.Content).all);
         end if;
      end;
      Free_Stream (S);
      Terminated := Value (R.Head).all'Length = 0;
   exception
      when GNAT.Sockets.Socket_Error =>
         --  This is what happens on VxWorks when the peer closes the
         --  socket; other OSs happily read an empty header.
         Terminated := True;
   end Initialize;


   function Get_Method (From : Request) return Method is
      Matches : GNAT.Regpat.Match_Array (0 .. URL_Max_Parens);
      Input : String renames Value (From.Head).all;
      use type GNAT.Regpat.Match_Location;
   begin
      GNAT.Regpat.Match (URL_Matcher, Input, Matches);
      if Matches (0) = GNAT.Regpat.No_Match then
         return "";
      else
         return To_String (Input, Matches (Method_Match));
      end if;
   end Get_Method;


   function Get_Version (From : Request) return Version is
      Matches : GNAT.Regpat.Match_Array (0 .. URL_Max_Parens);
      Input : String renames Value (From.Head).all;
      use type GNAT.Regpat.Match_Location;
   begin
      GNAT.Regpat.Match (URL_Matcher, Input, Matches);
      if Matches (0) = GNAT.Regpat.No_Match then
         return "";
      else
         return To_String (Input, Matches (Version_Match));
      end if;
   end Get_Version;


   function Get_URL (From : Request) return URL is
      Matches : GNAT.Regpat.Match_Array (0 .. URL_Max_Parens);
      Input : String renames Value (From.Head).all;
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
                          From : Request) return Property is
      Query_Matches : GNAT.Regpat.Match_Array (0 .. URL_Max_Parens);
      Query_Input : String renames Value (From.Head).all;
      Property_Matcher : constant GNAT.Regpat.Pattern_Matcher :=
        GNAT.Regpat.Compile ("(^|&)" & Named & "=([^&]*)",
                             Flags => GNAT.Regpat.Case_Insensitive);
      Property_Matches : GNAT.Regpat.Match_Array (0 .. 2);
      use type GNAT.Regpat.Match_Location;
   begin
      GNAT.Regpat.Match (URL_Matcher, Query_Input, Query_Matches);
      --  which has to succeed, we wouldn't get here with an illegal head
      pragma Assert (Query_Matches (0) /= GNAT.Regpat.No_Match);
      if To_String (Query_Input, Query_Matches (Method_Match)) = "GET"
        and then Query_Matches (Query_Match) /= GNAT.Regpat.No_Match then
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
      elsif To_String (Query_Input, Query_Matches (Method_Match))
        = "POST" and then Value (From.Content) /= null then
         declare
            Property_Input : String renames Value (From.Content).all;
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


   function Get_Field (Named : String; From : Request) return Property is
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
      GNAT.Regpat.Match (Field_Matcher, Value (From.Head).all, Matches);
      if Matches (0) = GNAT.Regpat.No_Match then
         return "";
      else
         return To_String (Value (From.Head).all, Matches (1));
      end if;
   end Get_Field;


   --  Content/attachment management  --

   function Get_Attachments (From : Request) return Attachments is
   begin
      return Attachments (From);
   end Get_Attachments;


   function Get_Field  (Named : String;
                        From : Attachments;
                        Index : Positive := 1) return Property is
   begin
      if Value (From.Content) = null then
         return "";
      else
         declare
            Part_Start : Positive;
            Part_Finish : Natural;
         begin
            Locate_Whole_Body_Part (From, Index, Part_Start, Part_Finish);
            declare
               Whole_Part : String
                 renames Value (From.Content)(Part_Start .. Part_Finish);
               Finish : constant Natural :=
                 Ada.Strings.Fixed.Index (Whole_Part,
                                          CRLF & CRLF);
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


   --  Binary content

   function Get_Content (From : Attachments;
                         Index : Positive := 1) return Contents is
   begin
      if Value (From.Content) = null then
         return Contents'(1 .. 0 => 0);
      else
         declare
            Part_Start : Positive;
            Part_Finish : Natural;
         begin
            Locate_Whole_Body_Part (From, Index, Part_Start, Part_Finish);
            declare
               Whole_Part : String
                 renames Value (From.Content)(Part_Start .. Part_Finish);
               Start : constant Natural :=
                 Ada.Strings.Fixed.Index (Whole_Part,
                                          CRLF & CRLF);
            begin
               --  Strip the header fields (if any) & the CRLF delimiter
               --  pair.
               declare
                  subtype Str is String (Start + 4 .. Whole_Part'Last);
                  subtype Arr is Contents (1 .. Str'Length);
                  function Conv is new Ada.Unchecked_Conversion (Str, Arr);
               begin
                  return Conv (Whole_Part (Start + 4 .. Whole_Part'Last));
               end;
            end;
         end;
      end if;
   end Get_Content;


   --  Text content

   procedure Open (C : in out Cursor;
                   From : Attachments;
                   Index : Positive := 1) is
   begin
      if C.Open then
         raise Status_Error;
      end if;
      C.Open := True;
      C.Line_Ending := Unknown;
      C.Data := From;
      Locate_Whole_Body_Part (From, Index, C.Start, C.Finish);
      --  Strip the header fields (if any) & the CRLF delimiter
      --  pair.
      C.Start := Ada.Strings.Fixed.Index
        (Value (From.Content)(C.Start .. C.Finish), CRLF & CRLF)
        + 4;
      C.Next := C.Start;
   end Open;


   procedure Close (C : in out Cursor) is
   begin
      if not C.Open then
         raise Status_Error;
      end if;
      C.Open := False;
      C.Data := (Head => Null_Pointer, Content => Null_Pointer);
   end Close;


   function At_End (C : Cursor) return Boolean is
   begin
      if not C.Open then
         raise Status_Error;
      end if;
      return C.Next > C.Finish;
   end At_End;


   procedure Get_Line (C : in out Cursor;
                       Line : out String;
                       Last : out Natural) is
   begin
      if not C.Open then
         raise Status_Error;
      end if;
      if C.Next > C.Finish then
         raise End_Error;
      end if;
      declare
         Text : String renames Value (C.Data.Content).all;
         CR : constant String := (1 => ASCII.CR);
         LF : constant String := (1 => ASCII.LF);
         Terminator : Natural;
      begin
         case C.Line_Ending is
            when Unknown =>
               declare
                  CR_Index : constant Natural :=
                    Ada.Strings.Fixed.Index (Text, CR, C.Next);
                  LF_Index : constant Natural :=
                    Ada.Strings.Fixed.Index (Text, LF, C.Next);
               begin
                  if CR_Index = 0 and LF_Index = 0 then
                     C.Line_Ending := Unterminated;
                  elsif CR_Index = 0 then
                     C.Line_Ending := Unix;
                  elsif LF_Index = 0 then
                     C.Line_Ending := Windows;
                  elsif CR_Index < LF_Index then
                     C.Line_Ending := Windows;
                  else
                     C.Line_Ending := Unix;
                  end if;
               end;
               Get_Line (C, Line, Last);  -- now we know what we have
               return;
            when Unterminated =>
               Terminator := 0;
            when Unix =>
               Terminator := Ada.Strings.Fixed.Index (Text, LF, C.Next);
            when Windows =>
               Terminator := Ada.Strings.Fixed.Index (Text, CR, C.Next);
         end case;
         if Terminator = 0 then
            --  the whole of the rest of the string is available
            Terminator := C.Finish + 1;
         end if;
         Last := Line'First - 1;
         while C.Next < Terminator loop
            exit when Last = Line'Last;
            Last := Last + 1;
            Line (Last) := Text (C.Next);
            C.Next := C.Next + 1;
         end loop;
         --  skip the LF or CRLF
         if C.Line_Ending = Unix then
            C.Next := C.Next + 1;
         elsif C.Line_Ending = Windows then
            C.Next := C.Next + 2;
         end if;
      end;
   end Get_Line;


   ---------------------------
   --  Response management  --
   ---------------------------

   function Find (For_Request : access Request) return Response'Class is
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

   function Response_Kind (This : Response) return String is
      pragma Unreferenced (This);
   begin
      return "200 OK";
   end Response_Kind;


   function Cacheable (This : Response) return Boolean is
      pragma Unreferenced (This);
   begin
      return True;
   end Cacheable;


   function Content_Type (This : Response) return String is
      pragma Unreferenced (This);
   begin
      return "text/plain";
   end Content_Type;


   function Content_Length (This : Response) return Integer is
   begin
      return Content (Response'Class (This))'Length;
      --  NB the dispatching call.
   end Content_Length;


   function Content (This : Response) return String is
      pragma Unreferenced (This);
   begin
      return "";
   end Content;


   procedure Write_Content (This : Response;
                            To : GNAT.Sockets.Socket_Type) is
      S : Stream_Access := Stream (To);
   begin
      String'Write (Stream (To), Content (Response'Class (This)));
      --  NB the dispatching call.
      Free_Stream (S);
   end Write_Content;


   procedure Respond (This : Response'Class;
                      To : GNAT.Sockets.Socket_Type) is
      S : Stream_Access := Stream (To);
   begin
      if Get_Version (This.To.all) = "1.0" then
         String'Write
           (S,
            "HTTP/1.0 " & Response_Kind (This) & CRLF &
              "Server: EWS" & CRLF &
              "Connection: close" & CRLF &
              "Content-Type: " & Content_Type (This) & CRLF &
              "Content-Length: " & Content_Length (This)'Img & CRLF);
      else
         String'Write
           (S,
            "HTTP/1.1 " & Response_Kind (This) & CRLF &
              "Server: EWS" & CRLF &
              "Content-Type: " & Content_Type (This) & CRLF &
              "Content-Length: " & Content_Length (This)'Img & CRLF);
      end if;
      if not Cacheable (This) then
         String'Write (S, "Cache-Control: no-cache" & CRLF);
      end if;
      String'Write (S, CRLF);
      Write_Content (This, To);
      Free_Stream (S);
   end Respond;


   ------------------------------------------------------
   --  Simple (error) responses and factory functions  --
   ------------------------------------------------------

   type Not_Found_Response (To : Request_P)
   is new Response (To) with null record;

   function Response_Kind (This : Not_Found_Response) return String;
   function Content (This : Not_Found_Response) return String;

   function Not_Found
     (R : access Request) return Response'Class is
   begin
      return Not_Found_Response'(To => Request_P (R));
   end Not_Found;


   type Not_Implemented_Response (To : Request_P)
   is new Response (To) with null record;

   function Response_Kind (This : Not_Implemented_Response) return String;
   function Content (This : Not_Implemented_Response) return String;

   function Not_Implemented
     (R : access Request) return Response'Class is
   begin
      return Not_Implemented_Response'(To => Request_P (R));
   end Not_Implemented;


   type Exception_Response_T (To : Request_P)
   is new Response (To) with record
         Info : Str.Bounded_String;
   end record;

   function Response_Kind (This : Exception_Response_T) return String;
   function Content (This : Exception_Response_T) return String;

   function Exception_Response
     (E : Ada.Exceptions.Exception_Occurrence;
      R : access Request) return Response'Class is
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

   function To_String
     (In_String : String;
      From : GNAT.Regpat.Match_Location) return String is
   begin
      return In_String (From.First .. From.Last);
   end To_String;


   function Unescape (S : String) return String is

      function Hex (H : String) return Natural;
      function Hex (H : String) return Natural is
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


   function Plus_To_Space (S : String) return String is
      Mapping : constant Ada.Strings.Maps.Character_Mapping
        := Ada.Strings.Maps.To_Mapping (From => "+",
                                        To => " ");
   begin
      return Ada.Strings.Fixed.Translate (S, Mapping);
   end Plus_To_Space;


   function Read_Request (From : Socket_Type) return String is
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
         Result : String (1 .. Natural (Last));
         pragma Import (Ada, Result);
         for Result'Address use Tmp'Address;
      begin
         return Result;
      end;

   end Read_Request;


   function Get_Content_Length (From : String) return Natural is
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


   procedure Locate_Whole_Body_Part (Within : Attachments;
                                     Index : Positive := 1;
                                     Start : out Positive;
                                     Finish : out Natural) is
      Content_Type : constant String := Get_Field ("Content-Type",
                                                   From => Request (Within));
      Text : String renames Value (Within.Content).all;
   begin
      if Content_Type'Length = 0
        or else Ada.Strings.Fixed.Index (Content_Type, "multipart") = 0 then
         Start := Text'First;
         Finish := Text'Last;
         return;
      end if;
      declare
         Marker : constant String := "boundary=";
         Boundary : constant String :=
           "--" &
           Content_Type (Ada.Strings.Fixed.Index (Content_Type, Marker)
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
            Finish :=
              Ada.Strings.Fixed.Index (Text, Boundary, Start);
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


   -----------------------------
   --  Error response bodies  --
   -----------------------------

   function Response_Kind (This : Not_Found_Response) return String is
      pragma Unreferenced (This);
   begin
      return "404 Not Found";
   end Response_Kind;

   function Content (This : Not_Found_Response) return String is
      pragma Unreferenced (This);
   begin
      return "Not found.";
   end Content;


   function Response_Kind (This : Not_Implemented_Response) return String is
      pragma Unreferenced (This);
   begin
      return "501 Not implemented";
   end Response_Kind;

   function Content (This : Not_Implemented_Response) return String is
      pragma Unreferenced (This);
   begin
      return "Not implemented.";
   end Content;


   function Response_Kind (This : Exception_Response_T) return String is
      pragma Unreferenced (This);
   begin
      return "500 Internal server error";
   end Response_Kind;

   function Content (This : Exception_Response_T) return String is
   begin
      return "Exception: " & Str.To_String (This.Info);
   end Content;


end EWS.HTTP;
