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
with EWS.Dynamic;
with EWS.HTTP;
with EWS.Types;
with GNAT.Calendar.Time_IO;

procedure EWS.Server.Test is

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

   function AJAX_Time
     (From_Request : HTTP.Request_P) return Dynamic.Dynamic_Response'Class;
   function AJAX_Time
     (From_Request : HTTP.Request_P) return Dynamic.Dynamic_Response'Class is
      Result : Dynamic.Dynamic_Response (From_Request);
   begin
      Dynamic.Set_Content_Type (Result, To => Types.Plain);
      Dynamic.Set_Content
        (Result,
         GNAT.Calendar.Time_IO.Image (Ada.Calendar.Clock, "%c"));
      return Result;
   end AJAX_Time;

begin

   Dynamic.Register (Dyn'Unrestricted_Access, "/test");
   Dynamic.Register (AJAX_Time'Unrestricted_Access, "/ajaxTime");
   Serve (Using_Port => 8080, Tracing => True);
   delay 1_000_000.0;

end EWS.Server.Test;
