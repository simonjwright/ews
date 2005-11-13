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

with Ada.Streams; use Ada.Streams;
with Ada.Strings.Unbounded.Aux;
with Ada.Unchecked_Deallocation;

package body EWS.Dynamic is


   --  Registry

   type URL_P is access constant HTTP.URL;

   type Cell;
   type Cell_P is access Cell;
   type Cell is record
      The_URL : URL_P;
      The_Creator : Creator;
      Next : Cell_P;
   end record;

   Registry : Cell_P;


   procedure Free_Stream
   is new Ada.Unchecked_Deallocation (Root_Stream_Type'Class,
                                      GNAT.Sockets.Stream_Access);


   procedure Append (This : in out Dynamic_Response;
                     Adding : String) is
   begin
      Ada.Strings.Unbounded.Append (This.Content, Adding);
   end Append;


   function Content_Type (This : Dynamic_Response) return String is
      Format_CSS : aliased constant String := "text/css";
      Format_GIF : aliased constant String := "image/gif";
      Format_HTML : aliased constant String := "text/html";
      Format_ICO : aliased constant String := "image/x-icon";
      Format_JPEG : aliased constant String := "image/jpeg";
      Format_Octet_Stream : aliased constant String
        := "application/octet-stream";
      Format_PNG : aliased constant String := "image/png";
      Format_Plain : aliased constant String := "text/plain";
      Type_Info : constant array (Types.Format) of Types.String_P
        := (Types.CSS => Format_CSS'Unchecked_Access,
            Types.HTML => Format_HTML'Unchecked_Access,
            Types.ICO => Format_ICO'Unchecked_Access,
            Types.GIF => Format_GIF'Unchecked_Access,
            Types.JPEG => Format_JPEG'Unchecked_Access,
            Types.OCTET_STREAM => Format_Octet_Stream'Unchecked_Access,
            Types.PNG => Format_PNG'Unchecked_Access,
            Types.Plain => Format_Plain'Unchecked_Access);
   begin
      return Type_Info (This.Form).all;
   end Content_Type;


   function Content_Length (This : Dynamic_Response) return Integer is
   begin
      return Ada.Strings.Unbounded.Length (This.Content);
   end Content_Length;


   function Find
     (For_Request : access HTTP.Request) return HTTP.Response'Class is
      For_URL : constant HTTP.URL := HTTP.Get_URL (For_Request.all);
      Reg : Cell_P := Registry;
   begin
      while Reg /= null loop
         if Reg.The_URL.all = For_URL then
            return Reg.The_Creator (HTTP.Request_P (For_Request));
         end if;
         Reg := Reg.Next;
      end loop;
      return HTTP.Not_Found (For_Request);
   end Find;


   procedure Register (The_Creator : Creator; For_The_URL : HTTP.URL) is
   begin
      Registry := new Cell'(The_URL => new HTTP.URL'(For_The_URL),
                            The_Creator => The_Creator,
                            Next => Registry);
   end Register;


   procedure Set_Content (This : in out Dynamic_Response;
                          To : String) is
   begin
      This.Content := Ada.Strings.Unbounded.To_Unbounded_String (To);
   end Set_Content;


   procedure Set_Content_Type (This : in out Dynamic_Response;
                               To : Types.Format) is
   begin
      This.Form := To;
   end Set_Content_Type;


   procedure Write_Content (This : Dynamic_Response;
                            To : GNAT.Sockets.Socket_Type) is
      --  We need to send only the contents, _without_ any bounds. So,
      --  we create a (sub)type with the correct bounds and use its
      --  'Write. There may well be constraint checks involved, but at
      --  least there should be no massive copies!
      use Ada.Strings.Unbounded;
      subtype This_String is String (1 .. Length (This.Content));
      S : GNAT.Sockets.Stream_Access := GNAT.Sockets.Stream (To);
   begin
      This_String'Write (S,
                         Aux.Get_String (This.Content).all);
      Free_Stream (S);
   end Write_Content;


end EWS.Dynamic;
