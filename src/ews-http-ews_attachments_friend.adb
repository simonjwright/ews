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

with Ada.Unchecked_Conversion;

package body EWS.HTTP.EWS_Attachments_Friend is


   function To_String_P is new Ada.Unchecked_Conversion (HTTP.String_P,
                                                         String_P);


   Empty_String : aliased constant String := "";

   procedure Get_Content (From : Attachments;
                          Index : Positive := 1;
                          Item : out String_P;
                          First : out Positive;
                          Last : out Natural) is
      CRLF2 : constant String := (ASCII.CR, ASCII.LF, ASCII.CR, ASCII.LF);
      use type Smart_Strings.Pointer;
   begin
      Item := To_String_P (Smart_Strings.Value (From.Content));
      if Item = null then
         Item := Empty_String'Access;
         First := Empty_String'First;
         Last := Empty_String'Last;
      else
         Locate_Whole_Body_Part (From, Index, First, Last);
         First := HTTP.Index (Item (First .. Last), CRLF2) + CRLF2'Length;
      end if;
   end Get_Content;


end EWS.HTTP.EWS_Attachments_Friend;
