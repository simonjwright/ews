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

--  This package provides an Input_Source which can be used by XMLAda
--  to populate a DOM from an XML section of an EWS.HTTP Attachment.

with EWS.HTTP.EWS_Attachments_Friend;
with Unicode;

package Input_Sources.EWS_Attachments is

   type Attachment_Input is new Input_Source with private;
   type Attachment_Input_Access is access all Attachment_Input'Class;
   --  A special implementation of a reader that reads from an attachment.

   procedure Open (From : EWS.HTTP.Attachments;
                   Section : Positive := 1;
                   Input : out Attachment_Input);
   --  Open the indicated attachment section for reading.
   --  This function can decode a file if it is coded in Utf8, Utf16 or Utf32

   procedure Close (Input : in out Attachment_Input);
   --  Close the input and free the memory

   procedure Next_Char
     (From : in out Attachment_Input;
      C    : out Unicode.Unicode_Char);
   --  Return the next character in the input.

   function Eof (From : Attachment_Input) return Boolean;
   --  True if From is past the last character in the file.

private

   type Attachment_Input is new Input_Sources.Input_Source with record
      Attachments : EWS.HTTP.Attachments;
      --  We keep Attachments so as to be sure that Buffer, which
      --  points to its contents, will remain valid (Attachments are
      --  reference-counted).
      Buffer : EWS.HTTP.EWS_Attachments_Friend.String_P;
      Buffer_First : Positive;
      Buffer_Last : Natural;
      Index : Natural;
   end record;

end Input_Sources.EWS_Attachments;
