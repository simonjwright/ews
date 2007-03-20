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
with Ada.Finalization;
with GNAT.Sockets;

package EWS.HTTP is

   --  Can't Elaborate_Body (body withs EWS.Static, EWS.Dynamic)


   --------------------------
   --  Request management  --
   --------------------------

   type Request is limited private;

   type Request_P is access all Request;

   procedure Initialize (R : out Request;
                         From : GNAT.Sockets.Socket_Type;
                         Terminated : out Boolean);

   subtype Method is String;
   --  GET or POST

   subtype Version is String;
   --  The HTTP version in use1.1 etc

   subtype URL is String;
   --  The file part of the request (eg, /index.html)

   subtype Property is String;
   --  The value of a parameter of the request (eg, in
   --  "/index.cgi?name=foo", "foo")
   --
   --  or
   --
   --  The value of a header field (eg, in "Content-Length: 1024",
   --  "1024")

   subtype Contents is String;
   --  The body of a request, not including any request parameters of
   --  header fields. In the case of a multipart message (as in file
   --  upload), the content of a particular part of the request.

   function Get_Method (From : Request) return Method;

   function Get_Version (From : Request) return Version;

   function Get_URL (From : Request) return URL;

   function Get_Property (Named : String; From : Request) return Property;
   --  Get the value of the named parameter of the query.

   function Get_Field (Named : String; From : Request) return Property;
   --  Get the value of the named header field of the query.

   function Get_Body_Field  (Named : String;
                             From : Request;
                             Index : Positive := 1) return Property;
   --  Get the value of the named header field of the Index'th part of
   --  the body of the request.

   function Get_Body_Content (From : Request;
                              Index : Positive := 1) return Contents;
   --  Get the contents of the Index'th part of the body of the request.

   ---------------------------
   --  Response management  --
   ---------------------------

   type Response (To : Request_P) is abstract tagged private;

   --  You do not need to override Content_Length and Write_Content
   --  provided you have overridden Content.

   function Response_Kind (This : Response) return String;
   --  default "200 OK"

   function Cacheable (This : Response) return Boolean;
   --  default True

   function Content_Type (This : Response) return String;
   --  default "text/plain"

   function Content_Length (This : Response) return Integer;
   --  default 0

   function Content (This : Response) return String;
   --  default "".

   procedure Write_Content (This : Response;
                            To : GNAT.Sockets.Socket_Type);

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

   type String_P is access String;

   type Request is new Ada.Finalization.Limited_Controlled with record
      Head : String_P;
      Content : String_P;
   end record;
   procedure Finalize (R : in out Request);

   type Response (To : Request_P) is abstract tagged null record;

end EWS.HTTP;
