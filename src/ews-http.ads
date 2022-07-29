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
--  Copyright (C) 2022, Stephane Carrez <Stephane.Carrez@gmail.com>

pragma Ada_2012;

with Ada.Exceptions;
with Ada.IO_Exceptions;
with Ada.Streams;
with GNAT.Sockets;

private with Ada.Finalization;
private with Ada.Strings.Maps;
private with EWS.Reference_Counted_Pointers_G;

package EWS.HTTP is

   --  Can't Elaborate_Body (body withs EWS.Static, EWS.Dynamic)


   --------------------------
   --  Request management  --
   --------------------------

   type Request is limited private;
   type Request_P is not null access all Request;

   procedure Initialize (R          : out Request;
                         From       :     GNAT.Sockets.Socket_Type;
                         Terminated : out Boolean);

   subtype Method is String;
   --  GET|POST|HEAD|PUT|DELETE|OPTIONS|PATCH

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

   function Keep_Alive_After_Responding (The_Request : Request) return Boolean;
   --  Returns True if the connection is to be left open after a
   --  normal completion (it's always closed after error).

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

   function Get_Field (Named : String;
                       From  : Attachments;
                       Index : Positive    := 1) return Property;
   --  Get the value of the named header field of the Index'th part of
   --  the attachments.


   --  String content
   ------------------

   type Contents is access constant String;
   --  The body of an attachment, not including any request parameters
   --  or header fields.

   type Content_Kind is (Binary, Text);

   function Get_Content (From  : Attachments;
                         Index : Positive    := 1) return Contents;
   --  Get the contents of the Index'th part of the attachment.

   function Get_Content_Kind (From  : Contents) return Content_Kind;


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

   procedure Open (C     : in out Cursor;
                   From  :        Attachments;
                   Index :        Positive := 1);
   --  Open a Cursor on the Index'th part of the attachments.
   --  Propagates Status_Error if the Cursor is already open.
   --  Propagates Name_Error if Index doesn't denote a part of the
   --  attachments.

   procedure Open (C     : in out Cursor;
                   From  :        Contents);
   --  Open a Cursor on From.
   --  Propagates Status_Error if the Cursor is already open.

   procedure Close (C : in out Cursor);
   --  Close a Cursor.
   --  Propagates Status_Error if the Cursor is already closed.

   function End_Of_File (C : Cursor) return Boolean;
   --  Return True if the Cursor has reached the end of its attachment.
   --  Propagates Status_Error if the Cursor is closed.

   procedure Get_Line (C    : in out Cursor;
                       Line :    out String;
                       Last :    out Natural);
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

   function Headers (This : Response) return String;
   --  Content-Type followed by Content-Length headers.

   procedure Write_Content
     (This :                 Response;
      To   : not null access Ada.Streams.Root_Stream_Type'Class);

   procedure Respond (This : Response'Class;
                      To   : GNAT.Sockets.Socket_Type);

   --  Factory function to create a Response

   function Find (For_Request : not null access Request) return Response'Class;

   --  Utilities for standard abnormal responses

   function Not_Found (R : not null access Request) return Response'Class;

   function Not_Implemented
     (R : not null access Request) return Response'Class;

   function Exception_Response
     (E : Ada.Exceptions.Exception_Occurrence;
      R : access Request) return Response'Class;


private

   --  Finalizable containment for strings used to hold a Request's
   --  head and content.
   type String_P is access String;
   package Smart_Strings
   is new Reference_Counted_Pointers_G (String, String_P);

   --  A Request isn't actually limited.
   type Request is record
      Head    : Smart_Strings.Pointer;
      Content : Smart_Strings.Pointer;
   end record;

   type Attachments is new Request;

   procedure Locate_Whole_Body_Part (Within :     Attachments;
                                     Index  :     Positive    := 1;
                                     Start  : out Positive;
                                     Finish : out Natural);
   --  Find the bounds in the Contents of Within of the Index'th
   --  part. The bounds include any leading properties.

   type Response (To : Request_P) is abstract tagged null record;

   type Line_Ending_Style is (Unknown, Unterminated, Unix, Windows);

   type Cursor is limited record
      Open        : Boolean           := False;
      Line_Ending : Line_Ending_Style := Unknown;
      Data        : Contents;
      Start       : Positive;
      Last        : Natural;
      Next        : Positive;
   end record;

   function Index
     (Source  : String;
      Pattern : String;
      Going   : Ada.Strings.Direction              := Ada.Strings.Forward;
      Mapping : Ada.Strings.Maps.Character_Mapping
        := Ada.Strings.Maps.Identity)
     return Natural;
   --  A replacement for the GNAT Ada05 Ada.Strings.Search.Index.

   --------------------------------
   --  Unbounded Memory Streams  --
   --------------------------------

   --  These are Streams held in memory.
   --
   --  The stream contents are held in chunks, allocated as required
   --  on Write.
   --
   --  There has to be a Read procedure, of course, but it's not
   --  intended to be used and will propagate Program_Error if called.
   --
   --  A Copy procedure is provided to copy the contents of the stream
   --  to a socket. Copy uses GNAT.Sockets.Send_Vector to send all the
   --  chunks to the socket in one call.
   --
   --  The chunks are freed when the Stream is finalized.

   subtype Stream_Chunk_Elements
   is Ada.Streams.Stream_Element_Array (0 .. 511);
   type Stream_Chunk;
   type Stream_Chunk_P is access Stream_Chunk;
   type Stream_Chunk is record
      Next     : Stream_Chunk_P;
      Elements : aliased Stream_Chunk_Elements;
   end record;

   type Unbounded_Memory_Stream;
   type Unbounded_Memory_Stream_Finalizer
     (UMS : not null access Unbounded_Memory_Stream)
     is new Ada.Finalization.Limited_Controlled with null record;
   overriding
   procedure Finalize (UMSF : in out Unbounded_Memory_Stream_Finalizer);

   type Unbounded_Memory_Stream
   is new Ada.Streams.Root_Stream_Type with record
      Finalizer : Unbounded_Memory_Stream_Finalizer
        (Unbounded_Memory_Stream'Access);
      Length    : Ada.Streams.Stream_Element_Offset := 0;
      Head      : Stream_Chunk_P;
      Tail      : Stream_Chunk_P;
   end record;
   not overriding
   procedure Copy  (Stream : Unbounded_Memory_Stream;
                    To     : GNAT.Sockets.Socket_Type);
   --  Read isn't meant to be called; output contents via Copy.
   overriding
   procedure Read  (Stream : in out Unbounded_Memory_Stream;
                    Item   :    out Ada.Streams.Stream_Element_Array;
                    Last   :    out Ada.Streams.Stream_Element_Offset);
   overriding
   procedure Write (Stream : in out Unbounded_Memory_Stream;
                    Item   :        Ada.Streams.Stream_Element_Array);


end EWS.HTTP;
