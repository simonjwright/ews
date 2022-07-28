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

with Ada.Dynamic_Priorities;
with Ada.Exceptions;
with Ada.Text_IO;
with EWS.HTTP;
with Interfaces.C.Strings;

package body EWS.Server is

   task type Server (With_Stack : Positive) is
      pragma Storage_Size (With_Stack);
      entry Start (Using_Port : GNAT.Sockets.Port_Type;
                   At_Priority : System.Priority;
                   Logging_Via : Logger;
                   Tracing : Boolean);
   end Server;
   type Server_P is access Server;

   procedure Respond (To : GNAT.Sockets.Socket_Type;
                      In_Sockets : in out GNAT.Sockets.Socket_Set_Type;
                      Logging_Via : Logger;
                      Tracing : Boolean);

   --  Logging/tracing
   procedure Default_Logger (Message : String; Level : Error_Level);
   procedure Log (Logging_Via : Logger;
                  Message : String;
                  With_Exception : Ada.Exceptions.Exception_Occurrence);
   function Resolve_Exception
     (Occurrence : Ada.Exceptions.Exception_Occurrence) return String;
   procedure Trace (Logging_Via : Logger;
                    Message : String;
                    Skt : GNAT.Sockets.Socket_Type;
                    Tracing : Boolean);
   procedure Trace (Logging_Via : Logger;
                    Message : String;
                    Tracing : Boolean);

   task body Server is
      Port : GNAT.Sockets.Port_Type;
      Priority : System.Priority;
      Logging_Via : Logger;
      Tracing : Boolean;
      Address : GNAT.Sockets.Sock_Addr_Type;
      Server_Socket : GNAT.Sockets.Socket_Type;
      Sockets : GNAT.Sockets.Socket_Set_Type;
      Write_Sockets : GNAT.Sockets.Socket_Set_Type; -- never used
      Selector : GNAT.Sockets.Selector_Type;
   begin
      pragma Warnings (Off, "call to obsolescent procedure *");
      GNAT.Sockets.Initialize;
      pragma Warnings (On, "call to obsolescent procedure *");
      accept Start (Using_Port : GNAT.Sockets.Port_Type;
                    At_Priority : System.Priority;
                    Logging_Via : Logger;
                    Tracing : Boolean) do
         Port := Using_Port;
         Priority := At_Priority;
         if Logging_Via = null then
            Server.Logging_Via := Default_Logger'Access;
         else
            Server.Logging_Via := Logging_Via;
         end if;
         Server.Tracing := Tracing;
      end Start;
      Ada.Dynamic_Priorities.Set_Priority (Priority);
      Address.Addr := GNAT.Sockets.Any_Inet_Addr;
      Address.Port := Port;
      GNAT.Sockets.Create_Socket (Server_Socket);
      GNAT.Sockets.Set_Socket_Option
        (Server_Socket,
         GNAT.Sockets.Socket_Level,
         (GNAT.Sockets.Reuse_Address, True));
      GNAT.Sockets.Bind_Socket (Server_Socket, Address);
      GNAT.Sockets.Listen_Socket (Server_Socket);
      GNAT.Sockets.Set (Sockets, Server_Socket);
      GNAT.Sockets.Create_Selector (Selector);
      loop
         declare
            Read_Sockets : GNAT.Sockets.Socket_Set_Type;
            Status : GNAT.Sockets.Selector_Status;
            use type GNAT.Sockets.Selector_Status;
         begin
            --  Initialize Read_Sockets with the sockets in use
            --  (Write_Sockets remains empty, we don't care if a
            --  socket becomes writable; we'll just block in that
            --  case).
            GNAT.Sockets.Copy (Sockets, Read_Sockets);
            --  Wait until something happens on one of the sockets.
            GNAT.Sockets.Check_Selector
              (Selector, Read_Sockets, Write_Sockets, Status);
            if Status = GNAT.Sockets.Completed then
               --  This was a successful completion. Find out which
               --  socket woke us up and deal with it. If there was
               --  more than one, we'll find out next time round the
               --  loop.
               declare
                  Socket : GNAT.Sockets.Socket_Type;
                  use type GNAT.Sockets.Socket_Type;
               begin
                  --  Which socket?
                  GNAT.Sockets.Get (Read_Sockets, Socket);
                  if Socket = Server_Socket then
                     --  It was the server; a new client has called
                     --  connect(). Accept the connection ...
                     GNAT.Sockets.Accept_Socket
                       (Server_Socket, Socket, Address);
                     Trace (Logging_Via, "connection", Socket, Tracing);
                     --  ... and add the new connected socket to the
                     --  set of sockets in use.
                     GNAT.Sockets.Set (Sockets, Socket);
                  elsif Socket = GNAT.Sockets.No_Socket then
                     --  None of our sockets has data/connection
                     --  available; don't care why.
                     Logging_Via ("server got No_Socket", Error);
                  else
                     --  There's an HTTP request to be read on one of
                     --  our connected clients' sockets; deal with it.
                     Trace (Logging_Via, "request", Socket, Tracing);
                     Respond (Socket, Sockets, Logging_Via, Tracing);
                  end if;
               end;
            else
               --  Unexpected, non-fatal error.
               Logging_Via ("server: Check_Selector returned " & Status'Img,
                            Error);
            end if;
            --  Clean up.
            GNAT.Sockets.Empty (Read_Sockets);
         exception
            when E : others =>
               Log (Logging_Via,
                    "server failed in inner loop",
                    With_Exception => E);
         end;
      end loop;
   exception
      when E : others =>
         Log (Logging_Via,
              "server task failed",
              With_Exception => E);
         GNAT.Sockets.Close_Socket (Server_Socket);
   end Server;


   procedure Serve
     (Using_Port : GNAT.Sockets.Port_Type;
      At_Priority : System.Priority := System.Default_Priority;
      With_Stack : Positive := 20_000;
      Logging_Via : Logger := null;
      Tracing : Boolean := False)
   is
      EWS_Server : constant Server_P := new Server (With_Stack);
   begin
      EWS_Server.Start (Using_Port, At_Priority, Logging_Via, Tracing);
   end Serve;


   procedure Respond (To : GNAT.Sockets.Socket_Type;
                      In_Sockets : in out GNAT.Sockets.Socket_Set_Type;
                      Logging_Via : Logger;
                      Tracing : Boolean)
   is

      procedure Close;
      procedure Close
      is
      begin
         GNAT.Sockets.Clear (In_Sockets, To);
         GNAT.Sockets.Close_Socket (To);
      end Close;

      Request : aliased HTTP.Request;
      Terminated : Boolean;

   begin

      begin
         HTTP.Initialize (Request, From => To, Terminated => Terminated);
      exception
         when E : others =>
            Log (Logging_Via,
                 "failed reading request",
                 With_Exception => E);
            return;
      end;

      if Terminated then
         Trace (Logging_Via, "connection terminated", Tracing);
         Close;
         return;
      end if;

      Trace (Logging_Via,
             "method "
               & HTTP.Get_Method (Request)
               & ", version "
               & HTTP.Get_Version (Request)
               & ", url "
               & HTTP.Get_URL (Request),
             Tracing);

      begin
         HTTP.Respond (HTTP.Find (Request'Unchecked_Access),
                       To => To);
      exception
         when GNAT.Sockets.Socket_Error =>
            --  Going to assume that a socket error occurs because of
            --  some browser behaviour (they've closed the socket
            --  without waiting for the response).
            Close;
            return;
         when E : others =>
            Log (Logging_Via, "failed in respond", With_Exception => E);
            begin
               HTTP.Respond
                 (HTTP.Exception_Response (E, Request'Unchecked_Access),
                  To => To);
            exception
               when others => null;
            end;
            Close;
            return;
      end;

      if not HTTP.Keep_Alive_After_Responding (Request) then
         Close;
      end if;

   end Respond;


   procedure Default_Logger (Message : String; Level : Error_Level)
   is
   begin
      Ada.Text_IO.Put_Line
        (Ada.Text_IO.Standard_Error, "EWS: " & Level'Img & ": " & Message);
   end Default_Logger;


   procedure Log (Logging_Via : Logger;
                  Message : String;
                  With_Exception : Ada.Exceptions.Exception_Occurrence)
   is
      use Ada.Exceptions;
   begin
      if Exception_Identity (With_Exception)
        = GNAT.Sockets.Socket_Error'Identity
      then
         begin
            Logging_Via
              (Message & ", "
                 & Resolve_Exception (With_Exception) & ", "
                 & Exception_Information (With_Exception),
               Error);
         exception
            --  If the special Socket_Error handling fails (XXX why
            --  would it?), revert to the standard case.
            when others =>
               Logging_Via (Message
                              & ", "
                              & Exception_Information (With_Exception),
                            Error);
         end;
      else
         Logging_Via (Message & ", " & Exception_Information (With_Exception),
                      Error);
      end if;
   end Log;


   function Resolve_Exception
     (Occurrence : Ada.Exceptions.Exception_Occurrence) return String
   is
      --  Fragmentarily copied from GNAT.Sockets function of the same
      --  name.
      Error : constant GNAT.Sockets.Error_Type
        := GNAT.Sockets.Resolve_Exception (Occurrence);
   begin
      case Error is
         when GNAT.Sockets.Cannot_Resolve_Error =>
            --  The errno is at the start of the exception message
            --  (inside []).
            declare
               function C_Strerror
                 (Errnum : Interfaces.C.int)
                 return Interfaces.C.Strings.chars_ptr;
               pragma Import (C, C_Strerror, "strerror");
               Msg : String
                 renames Ada.Exceptions.Exception_Message (Occurrence);
               First : Natural;
               Last : Natural;
               Errno : Integer;
               C_Msg : Interfaces.C.Strings.chars_ptr;
               use type Interfaces.C.Strings.chars_ptr;
            begin
               First := Msg'First;
               while First <= Msg'Last
                 and then Msg (First) not in '0' .. '9'
               loop
                  First := First + 1;
               end loop;

               if First > Msg'Last then
                  return "Cannot_Resolve_Error";
               end if;

               Last := First;
               while Last < Msg'Last
                 and then Msg (Last + 1) in '0' .. '9'
               loop
                  Last := Last + 1;
               end loop;

               Errno := Integer'Value (Msg (First .. Last));

               C_Msg := C_Strerror (Interfaces.C.int (Errno));

               if C_Msg = Interfaces.C.Strings.Null_Ptr then
                  return "unknown error " & Errno'Img;
               else
                  return Interfaces.C.Strings.Value (C_Msg);
               end if;
            end;
         when others =>
            return Error'Img;
      end case;
   end Resolve_Exception;


   procedure Trace (Logging_Via : Logger;
                    Message : String;
                    Skt : GNAT.Sockets.Socket_Type;
                    Tracing : Boolean)
   is
   begin
      if Tracing then
         Logging_Via
           (Message
              & ", socket"
              & GNAT.Sockets.Image (Skt)
              & " from "
              & GNAT.Sockets.Image (GNAT.Sockets.Get_Peer_Name (Skt)),
            Trace);
      end if;
   exception
      --  Only seen on Mandrake 10 on browser close. Leave it to
      --  Respond to clear up when it sees the send failure.
      when E : GNAT.Sockets.Socket_Error =>
         Log (Logging_Via,
              "failed in trace (" & Message & ")",
              With_Exception => E);
   end Trace;


   procedure Trace (Logging_Via : Logger;
                    Message : String;
                    Tracing : Boolean)
   is
   begin
      if Tracing then
         Logging_Via (Message, Trace);
      end if;
   end Trace;


end EWS.Server;
