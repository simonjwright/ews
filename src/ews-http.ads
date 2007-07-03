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
with Ada.IO_Exceptions;
with Ada.Strings.Maps;
with BC.Support.Smart_Pointers;
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
   --  The HTTP version in use; 1.1 etc

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

   function Get_Method (From : Request) return Method;

   function Get_Version (From : Request) return Version;

   function Get_URL (From : Request) return URL;

   function Get_Property (Named : String; From : Request) return Property;
   --  Get the value of the named parameter of the query.

   function Get_Field (Named : String; From : Request) return Property;
   --  Get the value of the named header field of the query.

   ---------------------
   --  Debug support  --
   ---------------------

   function Get_Head (From : Request) return String;
   function Get_Body (From : Request) return String;

   -------------------------------------
   --  Content/attachment management  --
   -------------------------------------

   type Attachments is private;
   --  The indexable collection of parts of a request body. In the
   --  case of a multipart message, as in file uploads, there will be
   --  several parts, the first (or only) part being index 1.

   function Get_Attachments (From : Request) return Attachments;

   procedure Clear (The_Attachments : in out Attachments);
   --  Attachments are organised using smart (reference counting)
   --  pointers. Use Clear to null out this particular reference after
   --  it's finished with.

   function Get_Field  (Named : String;
                        From : Attachments;
                        Index : Positive := 1) return Property;
   --  Get the value of the named header field of the Index'th part of
   --  the attachments.


   --  String content
   ------------------

   type Contents is access constant String;
   --  The body of an attachment, not including any request parameters
   --  of header fields.

   function Get_Content (From : Attachments;
                         Index : Positive := 1) return Contents;
   --  Get the contents of the Index'th part of the attachment.


   --  Text content
   ----------------

   --  Cursors are an analogue of Ada.Text_IO.File_Type, allowing
   --  reading lines from attachments.

   type Cursor is limited private;

   --  The following exceptions are propagated when the appropriate
   --  conditions occur.

   Status_Error : exception renames Ada.IO_Exceptions.Status_Error;
   Name_Error : exception renames Ada.IO_Exceptions.Name_Error;
   End_Error : exception renames Ada.IO_Exceptions.End_Error;

   procedure Open (C : in out Cursor;
                   From : Attachments;
                   Index : Positive := 1);
   --  Open a Cursor on the Index'th part of the attachments.
   --  Propagates Status_Error if the Cursor is already open.
   --  Propagates Name_Error if Index doesn't denote a part of the
   --  attachments.

   procedure Close (C : in out Cursor);
   --  Close a Cursor.
   --  Propagates Status_Error if the Cursor is already closed.

   function End_Of_File (C : Cursor) return Boolean;
   --  Return True if the Cursor has reached the end of its attachment.
   --  Propagates Status_Error if the Cursor is closed.

   procedure Get_Line (C : in out Cursor;
                       Line : out String;
                       Last : out Natural);
   --  Obtain the next line from the Cursor's attachment.
   --  Propagates Status_Error if the Cursor is closed.
   --  Propagates End_Error if the Cursor is already at the end.


   --  XML content
   ---------------

   --  The unit Input_Sources.EWS_Attachments is provided for use with
   --  XMLAda to process an attachment as XML.
   --
   --  In turn, it depends on the child unit
   --  EWS.HTTP.EWS_Attachments_Friend.


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
   package Smart_Strings
   is new BC.Support.Smart_Pointers (String, String_P);

   type Request is record
      Head : Smart_Strings.Pointer;
      Content : Smart_Strings.Pointer;
   end record;
   --  A Request isn't actually limited.

   type Attachments is new Request;

   procedure Locate_Whole_Body_Part (Within : Attachments;
                                     Index : Positive := 1;
                                     Start : out Positive;
                                     Finish : out Natural);
   --  Find the bounds in the Contents of Within of the Index'th
   --  part. The bounds include any leading properties.

   type Response (To : Request_P) is abstract tagged null record;

   type Line_Ending_Style is (Unknown, Unterminated, Unix, Windows);

   type Cursor is limited record
      Open : Boolean := False;
      Line_Ending : Line_Ending_Style;
      Data : Attachments;
      Start : Positive;
      Finish : Natural;
      Next : Positive;
   end record;

   function Index
     (Source  : String;
      Pattern : String;
      Going   : Ada.Strings.Direction := Ada.Strings.Forward;
      Mapping : Ada.Strings.Maps.Character_Mapping
        := Ada.Strings.Maps.Identity) return Natural;
   --  A replacement for the GNAT Ada05 Ada.Strings.Search.Index.

end EWS.HTTP;
