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

with Ada.Exceptions;
with Ada.Strings.Bounded;
with GNAT.Sockets;

package EWS.HTTP is

   --  Can't Elaborate_Body (body withs EWS.Static, EWS.Dynamic)


   --------------------------
   --  Request management  --
   --------------------------

   type Request is limited private;

   type Request_P is access all Request;

   procedure Initialize (R : out Request; From : GNAT.Sockets.Socket_Type);

   subtype URL is String;
   subtype Property is String;

   function Get_URL (From : Request) return URL;

   function Get_Property (Named : String;
                          From : Request) return Property;


   ---------------------------
   --  Response management  --
   ---------------------------

   type Response (To : Request_P) is abstract tagged private;

   type Response_P is access all Response;

   function Response_Kind (This : Response) return String;
   --  default "200 OK"

   function Content_Type (This : Response) return String;
   --  default "text/plain"

   function Content_Length (This : Response) return Integer;
   --  default 0

   procedure Write_Content (This : Response;
                            To : GNAT.Sockets.Socket_Type) is abstract;

   procedure Respond (This : Response'Class;
                      To : GNAT.Sockets.Socket_Type);

   --  Factory function to create a Response

   function Find (For_Request : access Request) return Response'Class;

   --  Utilities for standard abnormal responses

   function Not_Found (R : access Request) return Response'Class;

   function Not_Implemented (R : access Request) return Response'Class;

   function Exception_Response
     (E : Ada.Exceptions.Exception_Occurrence;
      R : access Request) return Response'Class;


private

   package Str is new Ada.Strings.Bounded.Generic_Bounded_Length (1024);

   type Request is limited record
      Head : Str.Bounded_String;
      Content : Str.Bounded_String;
   end record;

   type Response (To : Request_P) is abstract tagged null record;

end EWS.HTTP;
