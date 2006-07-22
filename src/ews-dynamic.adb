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

with Ada.Streams; use Ada.Streams;
with Ada.Unchecked_Deallocation;

package body EWS.Dynamic is


   --  Registry

   type URL_P is access constant HTTP.URL;

   type Cell;
   type Cell_P is access Cell;
   type Cell is record
      The_URL : URL_P;
      The_Creator : Creator;
      Next : Cell_P;
   end record;

   Registry : Cell_P;


   procedure Free
   is new Ada.Unchecked_Deallocation (String,
                                      String_P);

   procedure Free
   is new Ada.Unchecked_Deallocation (Root_Stream_Type'Class,
                                      GNAT.Sockets.Stream_Access);


   procedure Append (This : in out Dynamic_Response;
                     Adding : String) is
      use type Unbounded_String_Pointers.Pointer;
   begin
      if This.Content = Unbounded_String_Pointers.Null_Pointer then
         This.Content :=
           Unbounded_String_Pointers.Create (new Unbounded_String);
      end if;
      Append (Unbounded_String_Pointers.Value (This.Content).all, Adding);
   end Append;


   procedure Append (To : in out Unbounded_String; S : String) is
   begin
      if To.Last + S'Length > To.Buf'Length then
         declare
            Extended_Size : Positive := To.Buf'Length;
            New_Buffer : String_P;
         begin
            loop
               Extended_Size := Extended_Size * 2;
               exit when To.Last + S'Length <= Extended_Size;
            end loop;
            New_Buffer := new String (1 .. Extended_Size);
            New_Buffer (1 .. To.Last) := To.Buf (1 .. To.Last);
            Free (To.Buf);
            To.Buf := New_Buffer;
         end;
      end if;
      To.Buf (To.Last + 1 .. To.Last + S'Length) := S;
      To.Last := To.Last + S'Length;
   end Append;


   function Content_Type (This : Dynamic_Response) return String is
   begin
      return Types.Content_Type (This.Form);
   end Content_Type;


   function Content_Length (This : Dynamic_Response) return Integer is
   begin
      return Unbounded_String_Pointers.Value (This.Content).Last;
   end Content_Length;


   procedure Finalize (U : in out Unbounded_String) is
   begin
      if U.Buf /= null then
         Free (U.Buf);
      end if;
   end Finalize;


   function Find
     (For_Request : access HTTP.Request) return HTTP.Response'Class is
      For_URL : constant HTTP.URL := HTTP.Get_URL (For_Request.all);
      Reg : Cell_P := Registry;
   begin
      while Reg /= null loop
         if Reg.The_URL.all = For_URL then
            return Reg.The_Creator (HTTP.Request_P (For_Request));
         end if;
         Reg := Reg.Next;
      end loop;
      return HTTP.Not_Found (For_Request);
   end Find;


   procedure Initialize (U : in out Unbounded_String) is
   begin
      U.Buf := new String (1 .. 1024);
   end Initialize;


   procedure Register (The_Creator : Creator; For_The_URL : HTTP.URL) is
   begin
      Registry := new Cell'(The_URL => new HTTP.URL'(For_The_URL),
                            The_Creator => The_Creator,
                            Next => Registry);
   end Register;


   procedure Set_Content (This : in out Dynamic_Response;
                          To : String) is
   begin
      This.Content := Unbounded_String_Pointers.Create (new Unbounded_String);
      Append (This, To);
   end Set_Content;


   procedure Set_Content_Type (This : in out Dynamic_Response;
                               To : Types.Format) is
   begin
      This.Form := To;
   end Set_Content_Type;


   procedure Write_Content (This : Dynamic_Response;
                            To : GNAT.Sockets.Socket_Type) is
      --  We need to send only the contents, _without_ any bounds. So,
      --  we create a (sub)type with the correct bounds and use its
      --  'Write.
      Content : Unbounded_String
        renames Unbounded_String_Pointers.Value (This.Content).all;
      subtype This_String is String (1 .. Content.Last);
      S : GNAT.Sockets.Stream_Access := GNAT.Sockets.Stream (To);
   begin
      This_String'Write (S, Content.Buf (1 .. Content.Last));
      Free (S);
   end Write_Content;


end EWS.Dynamic;
