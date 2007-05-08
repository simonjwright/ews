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

--  $RCSfile$
--  $Revision$
--  $Date$
--  $Author$

with Ada.Calendar;
with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with EWS.Dynamic;
with EWS.HTTP;
with EWS.Types;
with GNAT.Calendar.Time_IO;

procedure EWS.Server.Test is

   --  Dynamic page
   function Dyn
     (From_Request : HTTP.Request_P) return Dynamic.Dynamic_Response'Class;

   function Dyn
     (From_Request : HTTP.Request_P) return Dynamic.Dynamic_Response'Class is
      Result : Dynamic.Dynamic_Response (From_Request);
   begin
      Dynamic.Set_Content_Type (Result, To => Types.HTML);
      Dynamic.Set_Content
        (Result,
         "<html><head>"
           & "<meta http-equiv=""Refresh"" content=""1""/>"
           & "<title>EWS dynamic page</title><head>"
           & "<body bgcolor=""gray"""
           & "<center>The time is <b>");
      Dynamic.Append
        (Result,
         Adding => GNAT.Calendar.Time_IO.Image (Ada.Calendar.Clock,
                                                "%c"));
      Dynamic.Append
        (Result,
         "</b></center>"
           & "</body>"
           & "</html>");
      return Result;
   end Dyn;

   --  AJAX page

   function AJAX_Change
     (From_Request : HTTP.Request_P)
     return Dynamic.Dynamic_Response'Class;

   function AJAX_Status
     (From_Request : HTTP.Request_P)
     return Dynamic.Dynamic_Response'Class;

   function AJAX_Time
     (From_Request : HTTP.Request_P)
     return Dynamic.Dynamic_Response'Class;

   function File_Input
     (From_Request : HTTP.Request_P)
     return Dynamic.Dynamic_Response'Class;

   function Upload_Result
     (For_Request : HTTP.Request_P;
      Message : String)
     return Dynamic.Dynamic_Response'Class;


   type Date_Format is (ISO, US, European, Locale);
   Current_Date_Format : Date_Format := ISO;


   function AJAX_Change
     (From_Request : HTTP.Request_P)
     return Dynamic.Dynamic_Response'Class is
      Result : Dynamic.Dynamic_Response (From_Request);
   begin
      declare
         Format_Property : constant String
           := EWS.HTTP.Get_Property ("format", From_Request.all);
      begin
         if Format_Property /= "" then
            Put_Line ("saw format=" & Format_Property);
            Current_Date_Format := Date_Format'Value (Format_Property);
         end if;
      end;
      Dynamic.Set_Content_Type (Result, To => Types.Plain);
      Dynamic.Set_Content (Result, "OK");
      return Result;
   end AJAX_Change;


   function AJAX_Status
     (From_Request : HTTP.Request_P)
     return Dynamic.Dynamic_Response'Class is
      Result : Dynamic.Dynamic_Response (From_Request);
   begin
      Dynamic.Set_Content_Type (Result, To => Types.XML);
      return Result;
   end AJAX_Status;


   function AJAX_Time
     (From_Request : HTTP.Request_P) return Dynamic.Dynamic_Response'Class is
      Result : Dynamic.Dynamic_Response (From_Request);
      function Format return GNAT.Calendar.Time_IO.Picture_String;
      function Format return GNAT.Calendar.Time_IO.Picture_String is
      begin
         case Current_Date_Format is
            when ISO => return GNAT.Calendar.Time_IO.ISO_Date;
            when US => return GNAT.Calendar.Time_IO.US_Date;
            when European => return GNAT.Calendar.Time_IO.European_Date;
            when Locale => return "%c";
         end case;
      end Format;
   begin
      Dynamic.Set_Content_Type (Result, To => Types.Plain);
      Dynamic.Set_Content
        (Result,
         GNAT.Calendar.Time_IO.Image (Ada.Calendar.Clock, Format));
      return Result;
   end AJAX_Time;


   function File_Input
     (From_Request : HTTP.Request_P) return Dynamic.Dynamic_Response'Class is
      C : HTTP.Cursor;
      N : Natural := 0;
      Line : String (1 .. 1024);
      Last : Natural;
   begin
      HTTP.Open (C, HTTP.Get_Attachments (From_Request.all));
      while not HTTP.End_Of_File (C) loop
         N := N + 1;
         Put (N'Img & ": ");
         HTTP.Get_Line (C, Line, Last);
         Put_Line (Line (1 .. Last));
      end loop;
      HTTP.Close (C);
      return Upload_Result
        (From_Request, "Upload complete," & N'Img & " lines.");
   exception
      when E : others =>
         begin
            HTTP.Close (C);
         exception
            when others => null;
         end;
         return Upload_Result
           (From_Request,
            "Upload failed: " & Ada.Exceptions.Exception_Information (E));
   end File_Input;

   function Upload_Result
     (For_Request : HTTP.Request_P;
      Message : String)
     return Dynamic.Dynamic_Response'Class is
      Result : Dynamic.Dynamic_Response (For_Request);
   begin
      Dynamic.Set_Content_Type (Result, To => Types.HTML);
      Dynamic.Append (Result, "<body onload=""alert('");
      for C in Message'Range loop
         case Message (C) is
            when ASCII.CR | ASCII.NUL => null;
            when ASCII.LF => Dynamic.Append (Result, "\n");
            when others => Dynamic.Append (Result, String'(1 => Message (C)));
         end case;
      end loop;
      Dynamic.Append (Result, "')"">");
      return Result;
   end Upload_Result;

begin

   Dynamic.Register (Dyn'Unrestricted_Access, "/test");
   Dynamic.Register (AJAX_Change'Unrestricted_Access, "/aChange");
   Dynamic.Register (AJAX_Status'Unrestricted_Access, "/state.xml");
   Dynamic.Register (AJAX_Time'Unrestricted_Access, "/ajaxTime");
   Dynamic.Register (File_Input'Unrestricted_Access, "/fileInput");
   Serve (Using_Port => 8080, Tracing => True);
   delay 1_000_000.0;

end EWS.Server.Test;
