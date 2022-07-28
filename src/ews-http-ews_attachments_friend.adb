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
--  Copyright (C) 2007-2022, Simon Wright <simon@pushface.org>

with Ada.Unchecked_Conversion;

package body EWS.HTTP.EWS_Attachments_Friend is


   function To_String_P is new Ada.Unchecked_Conversion (HTTP.String_P,
                                                         String_P);


   Empty_String : aliased constant String := "";

   procedure Get_Content (From : Attachments;
                          Index : Positive := 1;
                          Item : out String_P;
                          First : out Positive;
                          Last : out Natural)
   is
      CRLF2 : constant String := (ASCII.CR, ASCII.LF, ASCII.CR, ASCII.LF);
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
