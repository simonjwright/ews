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
--  Copyright (C) 2003-2022, Simon Wright <simon@pushface.org>

pragma Ada_2012;

with Ada.Streams; use Ada.Streams;
with Ada.Unchecked_Conversion;
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
   Default_Creator  : Creator;


   procedure Free
   is new Ada.Unchecked_Deallocation (String,
                                      String_P);


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


   procedure Append_Element (This : in out Dynamic_Response;
                             Element : String;
                             Content : String) is
      Str : Unbounded_String
        renames Unbounded_String_Pointers.Value (This.Content).all;
   begin
      Append (Str, "<");
      Append (Str, Element);
      Append (Str, ">");
      Append (Str, Content);
      Append (Str, "</");
      Append (Str, Element);
      Append (Str, ">");
   end Append_Element;


   function Cacheable (This : Dynamic_Response) return Boolean is
      pragma Unreferenced (This);
   begin
      return False;
   end Cacheable;


   function Content_Type (This : Dynamic_Response) return String is
   begin
      return Types.Content_Type (This.Form);
   end Content_Type;


   function Content_Length (This : Dynamic_Response) return Integer is
      use type Unbounded_String_Pointers.Pointer;
   begin
      if This.Content = Unbounded_String_Pointers.Null_Pointer then
         return 0;
      else
         return Unbounded_String_Pointers.Value (This.Content).Last;
      end if;
   end Content_Length;


   procedure Finalize (U : in out Unbounded_String) is
   begin
      if U.Buf /= null then
         Free (U.Buf);
      end if;
   end Finalize;


   function Find
     (For_Request : not null access HTTP.Request) return HTTP.Response'Class
   is
      For_URL : constant HTTP.URL := HTTP.Get_URL (For_Request.all);
      Reg : Cell_P := Registry;
   begin
      while Reg /= null loop
         if Reg.The_URL.all = For_URL then
            return Reg.The_Creator (HTTP.Request_P (For_Request));
         end if;
         Reg := Reg.Next;
      end loop;
      if Default_Creator /= null then
         return Default_Creator (HTTP.Request_P (For_Request));
      end if;
      return HTTP.Not_Found (For_Request);
   end Find;


   procedure Initialize (U : in out Unbounded_String) is
   begin
      U.Buf := new String (1 .. 256);
   end Initialize;


   procedure Register (The_Creator : not null Creator;
                       For_The_URL :          HTTP.URL) is
   begin
      Registry := new Cell'(The_URL => new HTTP.URL'(For_The_URL),
                            The_Creator => The_Creator,
                            Next => Registry);
   end Register;

   procedure Register_Default (The_Creator : not null Creator) is
   begin
      Default_Creator := The_Creator;
   end Register_Default;

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


   procedure Write (To : not null access Ada.Streams.Root_Stream_Type'Class;
                    U :           Unbounded_String)
   is
      subtype Input is String (1 .. U.Last);
      subtype Output is Ada.Streams.Stream_Element_Array
        (1 .. Stream_Element_Offset (U.Last));
      function Convert is new Ada.Unchecked_Conversion (Input, Output);
   begin
      Ada.Streams.Write (To.all, Convert (U.Buf (1 .. U.Last)));
   end Write;


   procedure Write_Content
     (This :                 Dynamic_Response;
      To   : not null access Ada.Streams.Root_Stream_Type'Class)
   is
      use type Unbounded_String_Pointers.Pointer;
   begin
      if This.Content /= Unbounded_String_Pointers.Null_Pointer then
         Unbounded_String'Write
           (To,
            Unbounded_String_Pointers.Value (This.Content).all);
      end if;
   end Write_Content;


end EWS.Dynamic;
