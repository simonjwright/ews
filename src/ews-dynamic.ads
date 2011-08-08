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

with Ada.Finalization;
with Ada.Streams;
with BC.Support.Smart_Pointers;
with EWS.HTTP;
with EWS.Types;

package EWS.Dynamic is

   pragma Elaborate_Body;

   function Find
     (For_Request : access HTTP.Request) return HTTP.Response'Class;

   type Dynamic_Response (R : HTTP.Request_P)
      is new HTTP.Response with private;

   type Creator
      is access function (From_Request : HTTP.Request_P)
                         return Dynamic_Response'Class;

   --  The server will call the given Creator when the given URL is
   --  requested.
   --  In "http://foo.com:1234/bar", for example, the URL is "/bar".
   procedure Register (The_Creator : Creator; For_The_URL : HTTP.URL);

   --  Operations callable by Creator functions.
   procedure Set_Content_Type (This : in out Dynamic_Response;
                               To   :        Types.Format);
   procedure Set_Content (This : in out Dynamic_Response;
                          To   :        String);
   procedure Append (This   : in out Dynamic_Response;
                     Adding :        String);

   --  Utility for HTML/XML, for adding a single element with text
   --  content. Add elements containing other elements "by hand".
   procedure Append_Element (This    : in out Dynamic_Response;
                             Element :        String;
                             Content :        String);

private

   --  Construct an Unbounded String implementation which can be used
   --  to hold the successively-appended parts of a Response.
   --
   --  In a quest for efficiency, the content holder
   --  (Unbounded_String.Buf ) is initialized to a standard size (256
   --  bytes, see body) and is doubled by reallocation whenever it
   --  overflows.
   --
   --  Unbounded_Strings are limited; they're held in a
   --  Dynamic_Response via a reference-counted pointer, using the
   --  Booch Components' Smart_Pointers package. When the last copy of
   --  a Dynamic_Response goes out of scope, the Unbounded_String it
   --  contains is freed, and finalization frees all its buffer
   --  memory.

   type String_P is access String;
   type Unbounded_String is new Ada.Finalization.Limited_Controlled with record
      Last : Natural := 0;
      Buf : String_P;
   end record;

   procedure Initialize (U : in out Unbounded_String);
   procedure Finalize (U : in out Unbounded_String);
   procedure Append (To : in out Unbounded_String; S : String);

   procedure Write (To : access Ada.Streams.Root_Stream_Type'Class;
                    U  :        Unbounded_String);
   for Unbounded_String'Write use Write;
   --  No need for a Read operation.

   type Unbounded_String_P is access Unbounded_String;

   package Unbounded_String_Pointers is new BC.Support.Smart_Pointers
     (Unbounded_String, Unbounded_String_P);

   type Dynamic_Response (R : HTTP.Request_P)
   is new HTTP.Response (R) with record
      Form : Types.Format := Types.Plain;
      Content : Unbounded_String_Pointers.Pointer;
   end record;

   function Cacheable (This : Dynamic_Response) return Boolean;
   function Content_Type (This : Dynamic_Response) return String;
   function Content_Length (This : Dynamic_Response) return Integer;
   procedure Write_Content (This :        Dynamic_Response;
                            To   : access Ada.Streams.Root_Stream_Type'Class);

end EWS.Dynamic;
