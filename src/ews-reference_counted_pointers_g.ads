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
--  Copyright (C) 2013-2022, Simon Wright <simon@pushface.org>

pragma Ada_2012;

private with Ada.Finalization;

generic
   type T (<>) is limited private;
   type P is access T;
package EWS.Reference_Counted_Pointers_G is

   type Pointer is private;
   --  A Pointer variable encapsulates a reference-counted accessor of
   --  type P (to a T).

   Null_Pointer : constant Pointer;
   --  Assign this to a Pointer when you've finished with its contents.

   --  function Create (Value : T) return Pointer;
   --  Returns a new encapsulation.

   function Create (Value : not null P) return Pointer;
   --  Returns a new encapsulation. You must NOT deallocate the Value
   --  passed; it will be deallocated when there are no more
   --  references to it.

   --  function Value (Ptr : Pointer) return T with Inline;
   --  Returns the encapsulated value.

   function Value (Ptr : Pointer) return P with Inline;
   --  Returns the encapsulated pointer.

private

   type Node is record
      Count : Natural := 0;
      Value : P;
   end record;
   type Ref is access Node;

   type Pointer is new Ada.Finalization.Controlled with record
      Rep : Ref;
   end record;

   procedure Adjust (Obj : in out Pointer);
   procedure Finalize (Obj : in out Pointer);

   Null_Pointer : constant Pointer
     := Pointer'(Ada.Finalization.Controlled with Rep => null);

end EWS.Reference_Counted_Pointers_G;
