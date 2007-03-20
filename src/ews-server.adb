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

with Ada.Dynamic_Priorities;
with Ada.Exceptions;
with Ada.Text_IO;
with EWS.HTTP;
with Interfaces.C.Strings;

package body EWS.Server is

   task type Server is
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
      GNAT.Sockets.Initialize;
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
            GNAT.Sockets.Copy (Sockets, Read_Sockets);
            GNAT.Sockets.Check_Selector
              (Selector, Read_Sockets, Write_Sockets, Status);
            if Status = GNAT.Sockets.Completed then
               declare
                  Socket : GNAT.Sockets.Socket_Type;
                  use type GNAT.Sockets.Socket_Type;
               begin
                  GNAT.Sockets.Get (Read_Sockets, Socket);
                  if Socket = Server_Socket then
                     GNAT.Sockets.Accept_Socket
                       (Server_Socket, Socket, Address);
                     Trace (Logging_Via, "connection", Socket, Tracing);
                     GNAT.Sockets.Set (Sockets, Socket);
                  elsif Socket = GNAT.Sockets.No_Socket then
                     Logging_Via ("server got No_Socket", Error);
                  else
                     Trace (Logging_Via, "request", Socket, Tracing);
                     Respond (Socket, Sockets, Logging_Via, Tracing);
                  end if;
               end;
            else
               Logging_Via (
                            "server: Check_Selector returned " & Status'Img,
                            Error);
            end if;
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
              "server failed in outer loop",
              With_Exception => E);
         GNAT.Sockets.Close_Socket (Server_Socket);
   end Server;


   procedure Serve
     (Using_Port : GNAT.Sockets.Port_Type;
      At_Priority : System.Priority := System.Default_Priority;
      Logging_Via : Logger := null;
      Tracing : Boolean := False) is
      New_Server : constant Server_P := new Server;
   begin
      New_Server.Start (Using_Port, At_Priority, Logging_Via, Tracing);
   end Serve;


   procedure Respond (To : GNAT.Sockets.Socket_Type;
                      In_Sockets : in out GNAT.Sockets.Socket_Set_Type;
                      Logging_Via : Logger;
                      Tracing : Boolean) is
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
            GNAT.Sockets.Clear (In_Sockets, To);
            GNAT.Sockets.Close_Socket (To);
            return;
      end;

      if Terminated then
         Trace (Logging_Via, "connection terminated", Tracing);
         GNAT.Sockets.Clear (In_Sockets, To);
         GNAT.Sockets.Close_Socket (To);
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
            GNAT.Sockets.Clear (In_Sockets, To);
            GNAT.Sockets.Close_Socket (To);
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
            GNAT.Sockets.Clear (In_Sockets, To);
            GNAT.Sockets.Close_Socket (To);
            return;
      end;

      if HTTP.Get_Version (Request) = "1.0" then
         GNAT.Sockets.Clear (In_Sockets, To);
         GNAT.Sockets.Close_Socket (To);
      end if;

   end Respond;


   procedure Default_Logger (Message : String; Level : Error_Level) is
   begin
      Ada.Text_IO.Put_Line
        (Ada.Text_IO.Standard_Error, "EWS: " & Level'Img & ": " & Message);
   end Default_Logger;


   procedure Log (Logging_Via : Logger;
                  Message : String;
                  With_Exception : Ada.Exceptions.Exception_Occurrence) is
      use Ada.Exceptions;
   begin
      if Exception_Identity (With_Exception)
        = GNAT.Sockets.Socket_Error'Identity then
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
     (Occurrence : Ada.Exceptions.Exception_Occurrence) return String is
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
                    Tracing : Boolean) is
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
                    Tracing : Boolean) is
   begin
      if Tracing then
         Logging_Via (Message, Trace);
      end if;
   end Trace;


end EWS.Server;
