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

--  $RCSfile$
--  $Revision$
--  $Date$
--  $Author$

with GNAT.IO; use GNAT.IO;

package body EWS_Pages_Support is


   function "+"
     (S : String)
     return Ada.Strings.Unbounded.Unbounded_String
     renames Ada.Strings.Unbounded.To_Unbounded_String;


   function "+"
     (S : Ada.Strings.Unbounded.Unbounded_String)
     return String
     renames Ada.Strings.Unbounded.To_String;


   procedure Output (This : access Context) is
   begin
      Put_Line ("with " & (+This.Text) & ";");
   end Output;


   procedure Output (This : access Variable) is
   begin
      null;
   end Output;


   procedure Output (This : access Code) is
   begin
      Put_Line (+This.Text);
   end Output;


   procedure Output (This : access Literal) is
   begin
      Put_Line ("Put_Line (""" & (+This.Text) & """);");
   end Output;


   procedure Add_Context (S : String; To : in out Compiled_Page) is
   begin
      Collections.Append
        (To.Context, new Context'(Text => +S));
   end Add_Context;


   procedure Add_Text (S : String; To : in out Compiled_Page) is
   begin
      Collections.Append
        (To.Text, new Literal'(Text => +S));
   end Add_Text;


   procedure Add_Code (S : String; To : in out Compiled_Page) is
   begin
      Collections.Append
        (To.Text, new Code'(Text => +S));
   end Add_Code;


   procedure Output (This : Compiled_Page) is
      Context : Abstract_Containers.Iterator'Class
        := Collections.New_Iterator (This.Context);
      Text : Abstract_Containers.Iterator'Class
        := Collections.New_Iterator (This.Text);
      use Abstract_Containers;
   begin
      while not Is_Done (Context) loop
         Output (Current_Item (Context));
         Next (Context);
      end loop;
      while not Is_Done (Text) loop
         Output (Current_Item (Text));
         Next (Text);
      end loop;
   end Output;


end EWS_Pages_Support;
