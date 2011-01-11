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

package body EWS.Types is

   function Content_Type (For_Format : Format) return String is
      Format_CSS : aliased constant String := "text/css";
      Format_GIF : aliased constant String := "image/gif";
      Format_HTML : aliased constant String := "text/html";
      Format_ICO : aliased constant String := "image/x-icon";
      Format_JPEG : aliased constant String := "image/jpeg";
      Format_JavaScript : aliased constant String := "text/javascript";
      Format_Octet_Stream : aliased constant String
        := "application/octet-stream";
      Format_PNG : aliased constant String := "image/png";
      Format_Plain : aliased constant String := "text/plain";
      Format_XML : aliased constant String := "text/xml";
      Format_XSL : aliased constant String := "text/xsl";
      Type_Info : constant array (Types.Format) of Types.String_P
        := (
            Types.CSS => Format_CSS'Unchecked_Access,
            Types.GIF => Format_GIF'Unchecked_Access,
            Types.HTML => Format_HTML'Unchecked_Access,
            Types.ICO => Format_ICO'Unchecked_Access,
            Types.JPEG => Format_JPEG'Unchecked_Access,
            Types.JavaScript => Format_JavaScript'Unchecked_Access,
            Types.Octet_Stream => Format_Octet_Stream'Unchecked_Access,
            Types.PNG => Format_PNG'Unchecked_Access,
            Types.Plain => Format_Plain'Unchecked_Access,
            Types.XML => Format_XML'Unchecked_Access,
            Types.XSL => Format_XSL'Unchecked_Access
           );
   begin
      return Type_Info (For_Format).all;
   end Content_Type;

end EWS.Types;
