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

with Ada.Streams;
with EWS.HTTP;
with EWS.Types;

package EWS.Static with Elaborate_Body is

   function Find
     (For_Request : access HTTP.Request) return HTTP.Response'Class;

   --  These are used by ews-make_htdocs to encode the pages.
   type URL_Info is record
      URL : Types.String_P;
      Doc : Types.Stream_Element_Array_P;
      Form : Types.Format;
   end record;
   type URL_Info_Array is array (Positive range <>) of URL_Info;
   type URL_Info_Array_P is access constant URL_Info_Array;

   --  This is called when the package that holds the encoded pages
   --  is initialized.
   procedure Register (Pages : URL_Info_Array_P);

private

   type Static_Response (R : HTTP.Request_P)
     is new HTTP.Response (R) with record
        Form    : Types.Format;
        Content : Types.Stream_Element_Array_P;
     end record;

   function Content_Type (This : Static_Response) return String;
   function Content_Length (This : Static_Response) return Integer;
   procedure Write_Content (This :        Static_Response;
                            To   : access Ada.Streams.Root_Stream_Type'Class);

end EWS.Static;
