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

private with Ada.Finalization;

generic
   type T (<>) is limited private;
   type P is access T;
package EWS.Reference_Counted_Pointers_G with Preelaborate is

   type Pointer is private;
   --  A Pointer variable encapsulates a reference-counted accessor of
   --  type P (to a T).

   Null_Pointer : constant Pointer;
   --  Assign this to a Pointer when you've finished with its contents.

   --  function Create (Value : T) return Pointer;
   --  Returns a new encapsulation.

   function Create (Value : P) return Pointer;
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
