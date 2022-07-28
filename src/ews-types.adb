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
--  Copyright (C) 2006-2022, Simon Wright <simon@pushface.org>

package body EWS.Types is

   function Content_Type (For_Format : Format) return String is
      Format_CSS : aliased constant String := "text/css";
      Format_GIF : aliased constant String := "image/gif";
      Format_HTML : aliased constant String := "text/html";
      Format_ICO : aliased constant String := "image/x-icon";
      Format_JPEG : aliased constant String := "image/jpeg";
      Format_JSON : aliased constant String := "application/json";
      Format_JavaScript : aliased constant String := "text/javascript";
      Format_Octet_Stream : aliased constant String
        := "application/octet-stream";
      Format_PDF : aliased constant String := "application/pdf";
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
            Types.JSON => Format_JSON'Unchecked_Access,
            Types.JavaScript => Format_JavaScript'Unchecked_Access,
            Types.Octet_Stream => Format_Octet_Stream'Unchecked_Access,
            Types.PDF => Format_PDF'Unchecked_Access,
            Types.PNG => Format_PNG'Unchecked_Access,
            Types.Plain => Format_Plain'Unchecked_Access,
            Types.XML => Format_XML'Unchecked_Access,
            Types.XSL => Format_XSL'Unchecked_Access
           );
   begin
      return Type_Info (For_Format).all;
   end Content_Type;

end EWS.Types;
