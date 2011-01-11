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

with Ada.Strings.Unbounded;
with BC.Containers.Collections.Unmanaged;
with GNAT.Regpat;

package EWS_Pages_Support is


   type Fragment is abstract tagged record
      Text : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   type Fragment_P is access Fragment'Class;

   procedure Output (This : access Fragment) is abstract;


   type Context is new Fragment with null record;

   procedure Output (This : access Context);


   type Variable is new Fragment with null record;

   procedure Output (This : access Variable);


   type Code is new Fragment with null record;

   procedure Output (This : access Code);


   type Literal is new Fragment with null record;

   procedure Output (This : access Literal);


   package Abstract_Containers
   is new BC.Containers (Fragment_P);

   package Abstract_Collections
   is new Abstract_Containers.Collections;

   package Collections
   is new Abstract_Collections.Unmanaged;


   type Compiled_Page is record
      Context : Collections.Collection;
      Text : Collections.Collection;
   end record;


   procedure Add_Context (S : String; To : in out Compiled_Page);

   procedure Add_Text (S : String; To : in out Compiled_Page);

   procedure Add_Code (S : String; To : in out Compiled_Page);


   procedure Output (This : Compiled_Page);


   function To_String
     (In_String : String;
      From : GNAT.Regpat.Match_Array;
      At_Location : Natural) return String;


end EWS_Pages_Support;
