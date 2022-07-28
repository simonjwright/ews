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

with Ada.Streams;

package EWS.Types is

   type String_P is access constant String;

   type Stream_Element_Array_P is
     access constant Ada.Streams.Stream_Element_Array;

   type Format is
     (
      CSS,
      GIF,
      HTML,
      ICO,
      JPEG,
      JSON,
      JavaScript,
      Octet_Stream,
      PDF,
      PNG,
      Plain,
      XML,
      XSL
     );

   function Content_Type (For_Format : Format) return String;

end EWS.Types;
