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

package body EWS.Server is

   task type Server is
      entry Start (Using_Port : GNAT.Sockets.Port_Type;
                   At_Priority : System.Priority;
                   Tracing : Boolean);
   end Server;
   type Server_P is access Server;

   procedure Log (S : String);
   procedure Log (S : String;
                  With_Exception : Ada.Exceptions.Exception_Occurrence);
   procedure Respond (To : GNAT.Sockets.Socket_Type;
                      In_Sockets : in out GNAT.Sockets.Socket_Set_Type;
                      Tracing : Boolean);
   procedure Trace (Str : String;
                    Skt : GNAT.Sockets.Socket_Type;
                    Tracing : Boolean);
   procedure Trace (S : String; Tracing : Boolean);

   task body Server is
      Port : GNAT.Sockets.Port_Type;
      Priority : System.Priority;
      Tracing : Boolean;
      Address : GNAT.Sockets.Sock_Addr_Type;
      Server_Socket : GNAT.Sockets.Socket_Type;
      Sockets : GNAT.Sockets.Socket_Set_Type;
      Selector : GNAT.Sockets.Selector_Type;
   begin
      GNAT.Sockets.Initialize;
      accept Start (Using_Port : GNAT.Sockets.Port_Type;
                    At_Priority : System.Priority;
                    Tracing : Boolean) do
         Port := Using_Port;
         Priority := At_Priority;
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
            Write_Sockets : GNAT.Sockets.Socket_Set_Type;
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
                     Trace ("connection", Socket, Tracing);
                     GNAT.Sockets.Set (Sockets, Socket);
                  elsif Socket = GNAT.Sockets.No_Socket then
                     Log ("server got No_Socket");
                  else
                     Trace ("request", Socket, Tracing);
                     Respond (Socket, Sockets, Tracing);
                  end if;
               end;
            else
               Log ("server: Check_Selector returned " & Status'Img);
            end if;
         end;
      end loop;
   exception
      when E : others =>
         Log ("server failed in outer loop", With_Exception => E);
         GNAT.Sockets.Close_Socket (Server_Socket);
   end Server;


   procedure Serve
     (Using_Port : GNAT.Sockets.Port_Type;
      At_Priority : System.Priority := System.Default_Priority;
      Tracing : Boolean := False) is
      New_Server : constant Server_P := new Server;
   begin
      New_Server.Start (Using_Port, At_Priority, Tracing);
   end Serve;


   procedure Log (S : String) is
   begin
      Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, "EWS: " & S);
   end Log;


   procedure Log (S : String;
                  With_Exception : Ada.Exceptions.Exception_Occurrence) is
      use Ada.Exceptions;
   begin
      if Exception_Identity (With_Exception)
        = GNAT.Sockets.Socket_Error'Identity then
         Log (S
                & ", "
                & GNAT.Sockets.Resolve_Exception (With_Exception)'Img
                & ", "
                & Exception_Information (With_Exception));
      else
         Log (S & ", " & Exception_Information (With_Exception));
      end if;
   end Log;


   procedure Respond (To : GNAT.Sockets.Socket_Type;
                      In_Sockets : in out GNAT.Sockets.Socket_Set_Type;
                      Tracing : Boolean) is
      Request : aliased HTTP.Request;
      Terminated : Boolean;
   begin

      begin
         HTTP.Initialize (Request, From => To, Terminated => Terminated);
      exception
         when E : others =>
            Log ("failed reading request", With_Exception => E);
            GNAT.Sockets.Clear (In_Sockets, To);
            GNAT.Sockets.Close_Socket (To);
            return;
      end;

      if Terminated then
         Trace ("connection terminated", Tracing);
         GNAT.Sockets.Clear (In_Sockets, To);
         GNAT.Sockets.Close_Socket (To);
         return;
      end if;

      Trace ("method "
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
         when E : GNAT.Sockets.Socket_Error =>
            Log ("failed in respond", With_Exception => E);
            GNAT.Sockets.Clear (In_Sockets, To);
            GNAT.Sockets.Close_Socket (To);
            return;
         when E : others =>
            Log ("failed in respond", With_Exception => E);
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


   procedure Trace (Str : String;
                    Skt : GNAT.Sockets.Socket_Type;
                    Tracing : Boolean) is
   begin
      if Tracing then
         Log (Str
                & ", socket"
                & GNAT.Sockets.Image (Skt)
                & " from "
                & GNAT.Sockets.Image (GNAT.Sockets.Get_Peer_Name (Skt)));
      end if;
   end Trace;


   procedure Trace (S : String; Tracing : Boolean) is
   begin
      if Tracing then
         Log (S);
      end if;
   end Trace;


end EWS.Server;
