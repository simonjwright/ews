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
with Ada.Streams; use Ada.Streams;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with GNAT.Regpat;

with EWS.Dynamic;
with EWS.Static;

package body EWS.HTTP is


   use GNAT.Sockets;


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

   procedure Free_Stream
   is new Ada.Unchecked_Deallocation (Root_Stream_Type'Class, Stream_Access);


   -------------------------
   --  Public operations  --
   -------------------------

   procedure Initialize (R : out Request;
                         From : GNAT.Sockets.Socket_Type;
                         Terminated : out Boolean) is
   begin
      declare
         Head : constant String := Read_Request (From);
         Content : String (1 .. Get_Content_Length (Head));
         S : Stream_Access := Stream (From);
         use type GNAT.Sockets.Error_Type;
      begin
         R.Head := Str.To_Bounded_String (Head);
         String'Read (S, Content);
         R.Content := Str.To_Bounded_String (Content);
         Free_Stream (S);
         Terminated := Head'Length = 0;
      exception
         when E : GNAT.Sockets.Socket_Error =>
            if GNAT.Sockets.Resolve_Exception (E)
              = GNAT.Sockets.Connection_Reset_By_Peer then
               --  This is what happens on VxWorks; other OSs happily
               --  read an empty header.
               Terminated := True;
            else
               raise;
            end if;
      end;
   end Initialize;


   function Get_Method (From : Request) return Method is
      Matches : GNAT.Regpat.Match_Array (0 .. URL_Max_Parens);
      Input : constant String := Str.To_String (From.Head);
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
      Input : constant String := Str.To_String (From.Head);
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
      Input : constant String := Str.To_String (From.Head);
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
      Query_Input : constant String := Str.To_String (From.Head);
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
        = "POST" then
         declare
            Property_Input : constant String := Str.To_String (From.Content);
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


   ---------------------------
   --  Response management  --
   ---------------------------

   CRLF : constant String := CR & LF;


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
      Mapping : constant Character_Mapping := To_Mapping (From => "+",
                                                           To => " ");
   begin
      return Translate (S, Mapping);
   end Plus_To_Space;


   function Read_Request (From : Socket_Type) return String is
      Tmp : Stream_Element_Array (1 .. 2048);
      Last : Stream_Element_Offset := Tmp'First - 1;
      Next : Stream_Element_Offset;
      Termination : constant Stream_Element_Array :=
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
