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

with Ada.Streams;
with EWS.HTTP;
with EWS.Types;

private with Ada.Finalization;
private with EWS.Reference_Counted_Pointers_G;

package EWS.Dynamic with Elaborate_Body is

   function Find
     (For_Request : not null access HTTP.Request) return HTTP.Response'Class;

   type Dynamic_Response (R : HTTP.Request_P)
   is new HTTP.Response with private;

   type Creator
   is access function (From_Request : HTTP.Request_P)
                      return Dynamic_Response'Class;

   --  The server will call the given Creator when the given URL is
   --  requested.
   --  In "http://foo.com:1234/bar", for example, the URL is "/bar".
   procedure Register (The_Creator : not null Creator;
                       For_The_URL :          HTTP.URL);
   procedure Register_Default (The_Creator : not null Creator);

   --  Operations callable by Creator functions.
   not overriding
   procedure Set_Content_Type (This : in out Dynamic_Response;
                               To   :        Types.Format);
   not overriding
   procedure Set_Content (This : in out Dynamic_Response;
                          To   :        String);
   not overriding
   procedure Append (This   : in out Dynamic_Response;
                     Adding :        String);

   --  Utility for HTML/XML, for adding a single element with text
   --  content. Add elements containing other elements "by hand".
   not overriding
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
   --  Dynamic_Response via a reference-counted pointer. When the last
   --  copy of a Dynamic_Response goes out of scope, the
   --  Unbounded_String it contains is freed, and finalization frees
   --  all its buffer memory.

   type String_P is access String;
   type Unbounded_String is new Ada.Finalization.Limited_Controlled with record
      Last : Natural := 0;
      Buf : String_P;
   end record;

   overriding
   procedure Initialize (U : in out Unbounded_String);
   overriding
   procedure Finalize (U : in out Unbounded_String);
   not overriding
   procedure Append (To : in out Unbounded_String; S : String);

   procedure Write (To : not null access Ada.Streams.Root_Stream_Type'Class;
                    U  :                 Unbounded_String);
   for Unbounded_String'Write use Write;
   --  No need for a Read operation.

   type Unbounded_String_P is access Unbounded_String;

   package Unbounded_String_Pointers is new Reference_Counted_Pointers_G
     (Unbounded_String, Unbounded_String_P);

   type Dynamic_Response (R : HTTP.Request_P)
   is new HTTP.Response (R) with record
      Form : Types.Format := Types.Plain;
      Content : Unbounded_String_Pointers.Pointer;
   end record;

   overriding
   function Cacheable (This : Dynamic_Response) return Boolean;
   overriding
   function Content_Type (This : Dynamic_Response) return String;
   overriding
   function Content_Length (This : Dynamic_Response) return Integer;
   overriding
   procedure Write_Content
     (This :                 Dynamic_Response;
      To   : not null access Ada.Streams.Root_Stream_Type'Class);

end EWS.Dynamic;
