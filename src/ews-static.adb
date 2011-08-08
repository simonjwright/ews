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

with EWS.Htdocs;

package body EWS.Static is


   function Content_Type (This : Static_Response) return String
   is
   begin
      return Types.Content_Type (This.Form);
   end Content_Type;


   function Content_Length (This : Static_Response) return Integer
   is
   begin
      return This.Content'Length;
   end Content_Length;


   procedure Write_Content (This :        Static_Response;
                            To   : access Ada.Streams.Root_Stream_Type'Class)
   is
   begin
      Ada.Streams.Write (Stream => To.all,
                         Item => This.Content.all);
   end Write_Content;


   function Find
     (For_Request : access HTTP.Request) return HTTP.Response'Class
   is
      subtype Index is Natural range 0 .. Htdocs.Static_Urls'Last;
      function Find (For_URL : String) return Index;
      function Find (For_URL : String) return Index is
      begin
         for I in Htdocs.Static_Urls'Range loop
            if Htdocs.Static_Urls (I).URL.all = For_URL then
               return I;
            end if;
         end loop;
         return 0;
      end Find;
      For_URL : constant HTTP.URL := HTTP.Get_URL (For_Request.all);
      Location : Index;
   begin
      if For_URL'Length = 0 or else For_URL (For_URL'Last) = '/' then
         Location := Find (For_URL & "index.html");
         if Location > 0 then
            return Static_Response'
              (HTTP.Response with
               R => HTTP.Request_P (For_Request),
               Form => Htdocs.Static_Urls (Location).Form,
               Content => Htdocs.Static_Urls (Location).Doc);
         end if;
         Location := Find (For_URL & "index.htm");
         if Location > 0 then
            return Static_Response'
              (HTTP.Response with
               R => HTTP.Request_P (For_Request),
               Form => Htdocs.Static_Urls (Location).Form,
               Content => Htdocs.Static_Urls (Location).Doc);
         end if;
         Location := Find (For_URL & "default.html");
         if Location > 0 then
            return Static_Response'
              (HTTP.Response with
               R => HTTP.Request_P (For_Request),
               Form => Htdocs.Static_Urls (Location).Form,
               Content => Htdocs.Static_Urls (Location).Doc);
         end if;
         Location := Find (For_URL & "default.htm");
         if Location > 0 then
            return Static_Response'
              (HTTP.Response with
               R => HTTP.Request_P (For_Request),
               Form => Htdocs.Static_Urls (Location).Form,
               Content => Htdocs.Static_Urls (Location).Doc);
         end if;
      end if;
      Location := Find (For_URL);
      if Location > 0 then
         return Static_Response'
           (HTTP.Response with
            R => HTTP.Request_P (For_Request),
            Form => Htdocs.Static_Urls (Location).Form,
            Content => Htdocs.Static_Urls (Location).Doc);
      end if;
      return HTTP.Not_Found (For_Request);
   end Find;


end EWS.Static;
