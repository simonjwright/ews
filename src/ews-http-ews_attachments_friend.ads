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

--  This package cooperates with Input_Sources.EWS_Attachments to
--  allow XMLAda to populate a DOM from an XML section of an EWS.HTTP
--  Attachment.

package EWS.HTTP.EWS_Attachments_Friend is

   type String_P is access constant String;

   procedure Get_Content (From : Attachments;
                          Index : Positive := 1;
                          Item : out String_P;
                          First : out Positive;
                          Last : out Natural);

end EWS.HTTP.EWS_Attachments_Friend;
