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

--  This package provides an Input_Source which can be used by XMLAda
--  to populate a DOM from an XML section of an EWS.HTTP Attachment.

pragma Ada_2012;

with EWS.HTTP;
with Unicode;

private with EWS.HTTP.EWS_Attachments_Friend;

package Input_Sources.EWS_Attachments is

   type Attachment_Input is new Input_Source with private;
   type Attachment_Input_Access
      is not null access all Attachment_Input'Class;
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
