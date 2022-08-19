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

pragma Ada_2012;

with Ada.Streams;
with EWS.HTTP;
with EWS.Types;

package EWS.Static with Elaborate_Body is

   function Find
     (For_Request : not null access HTTP.Request) return HTTP.Response'Class;

   --  These are used by ews_generator to encode the pages.
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

   overriding
   function Content_Type (This : Static_Response) return String;
   overriding
   function Content_Length (This : Static_Response) return Integer;
   overriding
   procedure Write_Content
     (This :                 Static_Response;
      To   : not null access Ada.Streams.Root_Stream_Type'Class);

end EWS.Static;
