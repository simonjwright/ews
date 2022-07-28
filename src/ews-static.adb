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

package body EWS.Static is


   Pages : URL_Info_Array_P;

   procedure Register (Pages : URL_Info_Array_P)
   is
   begin
      Static.Pages := Register.Pages;
   end Register;


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


   procedure Write_Content
     (This :                 Static_Response;
      To   : not null access Ada.Streams.Root_Stream_Type'Class)
   is
   begin
      Ada.Streams.Write (Stream => To.all,
                         Item => This.Content.all);
   end Write_Content;


   function Find
     (For_Request : not null access HTTP.Request) return HTTP.Response'Class
   is
   begin
      if Pages = null then
         return HTTP.Not_Found (For_Request);
      else
         declare
            subtype Index is Natural range 0 .. Pages'Last;
            function Find (For_URL : String) return Index;
            function Find (For_URL : String) return Index is
            begin
               for I in Pages'Range loop
                  if Pages (I).URL.all = For_URL then
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
                     Form => Pages (Location).Form,
                     Content => Pages (Location).Doc);
               end if;
               Location := Find (For_URL & "index.htm");
               if Location > 0 then
                  return Static_Response'
                    (HTTP.Response with
                     R => HTTP.Request_P (For_Request),
                     Form => Pages (Location).Form,
                     Content => Pages (Location).Doc);
               end if;
               Location := Find (For_URL & "default.html");
               if Location > 0 then
                  return Static_Response'
                    (HTTP.Response with
                     R => HTTP.Request_P (For_Request),
                     Form => Pages (Location).Form,
                     Content => Pages (Location).Doc);
               end if;
               Location := Find (For_URL & "default.htm");
               if Location > 0 then
                  return Static_Response'
                    (HTTP.Response with
                     R => HTTP.Request_P (For_Request),
                     Form => Pages (Location).Form,
                     Content => Pages (Location).Doc);
               end if;
            end if;
            Location := Find (For_URL);
            if Location > 0 then
               return Static_Response'
                 (HTTP.Response with
                  R => HTTP.Request_P (For_Request),
                  Form => Pages (Location).Form,
                  Content => Pages (Location).Doc);
            end if;
            return HTTP.Not_Found (For_Request);
         end;
      end if;
   end Find;


end EWS.Static;
