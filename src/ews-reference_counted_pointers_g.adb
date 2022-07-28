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

with Ada.Unchecked_Deallocation;

package body EWS.Reference_Counted_Pointers_G is

   function Create (Value : not null P) return Pointer
   is
   begin
      return Pointer'(Ada.Finalization.Controlled
                      with Rep => new Node'(Count => 1,
                                            Value => Value));
   end Create;

   function Value (Ptr : Pointer) return P is
   begin
      if Ptr.Rep = null then
         return null;
      else
         return Ptr.Rep.Value;
      end if;
   end Value;

   procedure Adjust (Obj : in out Pointer) is
   begin
      if Obj.Rep /= null then
         Obj.Rep.Count := Obj.Rep.Count + 1;
      end if;
   end Adjust;

   procedure Delete is new Ada.Unchecked_Deallocation (T, P);
   procedure Delete is new Ada.Unchecked_Deallocation (Node, Ref);

   --  Finalize may be called more than once on the same object.
   --
   --  The first time it's called, we may set Tmp to a non-null value
   --  which designates the actual shared object and then proceed to
   --  decrement the count and, if no references remain, delete the
   --  used memory. But, in any case, *this* smart pointer no longer
   --  references the actual object, so another call to Finalize will
   --  have no effect.
   procedure Finalize (Obj : in out Pointer) is
      Tmp : Ref := Obj.Rep;
   begin
      Obj.Rep := null;
      if Tmp /= null then
         Tmp.Count := Tmp.Count - 1;
         if Tmp.Count = 0 then
            Delete (Tmp.Value);
            Delete (Tmp);
         end if;
      end if;
   end Finalize;


end EWS.Reference_Counted_Pointers_G;
