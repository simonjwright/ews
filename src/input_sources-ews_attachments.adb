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

with Unicode.CES.Utf16;
with Unicode.CES.Utf32;
with Unicode.CES.Utf8;

with Ada.Text_IO; use Ada.Text_IO;

--  This package provides an Input_Source which can be used by XMLAda
--  to populate a DOM from an XML section of an EWS.HTTP Attachment.

package body Input_Sources.EWS_Attachments is


   procedure Open (From : EWS.HTTP.Attachments;
                   Section : Positive := 1;
                   Input : out Attachment_Input) is
      Bom : Unicode.CES.Bom_Type;
   begin
      Input.Attachments := From;
      EWS.HTTP.EWS_Attachments_Friend.Get_Content
        (Input.Attachments,
         Section,
         Input.Buffer,
         Input.Buffer_First,
         Input.Buffer_Last);
      --  Check for the BOM
      Unicode.CES.Read_Bom
        (Input.Buffer (Input.Buffer_First .. Input.Buffer_Last),
         Input.Prolog_Size,
         Bom);
      case Bom is
         when Unicode.CES.Utf32_LE =>
            Set_Encoding (Input, Unicode.CES.Utf32.Utf32_LE_Encoding);
         when Unicode.CES.Utf32_BE =>
            Set_Encoding (Input, Unicode.CES.Utf32.Utf32_BE_Encoding);
         when Unicode.CES.Utf16_LE =>
            Set_Encoding (Input, Unicode.CES.Utf16.Utf16_LE_Encoding);
         when Unicode.CES.Utf16_BE =>
            Set_Encoding (Input, Unicode.CES.Utf16.Utf16_BE_Encoding);
         when Unicode.CES.Ucs4_BE
           | Unicode.CES.Ucs4_LE
           | Unicode.CES.Ucs4_2143
           | Unicode.CES.Ucs4_3412 =>
            raise Unicode.CES.Invalid_Encoding;
         when Unicode.CES.Utf8_All
           | Unicode.CES.Unknown =>
            Set_Encoding (Input, Unicode.CES.Utf8.Utf8_Encoding);
      end case;
      Input.Index := Input.Buffer_First + Input.Prolog_Size;
      Put_Line ("first" & Input.Buffer_First'Img & "; prolog_size" & Input.Prolog_Size'Img & "; first '" & Input.Buffer (Input.Index) & "'");
      Put_Line ("input |" & Input.Buffer (Input.Buffer_First .. Input.Buffer_Last) & "|");
   end Open;


   procedure Close (Input : in out Attachment_Input) is
   begin
      Input.Buffer := null;
      Input.Index := Natural'Last;
      EWS.HTTP.Clear (Input.Attachments);
   end Close;


   procedure Next_Char
     (From : in out Attachment_Input;
      C    : out Unicode.Unicode_Char) is
   begin
      Put ("reading at" & From.Index'Img & " '" & From.Buffer (From.Index) & "' -- ");
      From.Es.Read (From.Buffer.all, From.Index, C);
      Put_Line ("next '" & From.Buffer (From.Index) & "'");
      C := From.Cs.To_Unicode (C);
   end Next_Char;


   function Eof (From : Attachment_Input) return Boolean is
      use type EWS.HTTP.EWS_Attachments_Friend.String_P;
   begin
      return From.Buffer /= null and then From.Index > From.Buffer_Last;
   end Eof;


end Input_Sources.EWS_Attachments;
