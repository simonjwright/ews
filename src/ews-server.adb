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
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Sockets; use GNAT.Sockets;

with EWS.HTTP;

package body EWS.Server is

   task type Server is
      entry Start (Using_Port : GNAT.Sockets.Port_Type;
                   At_Priority : System.Priority);
   end Server;
   type Server_P is access Server;


   task body Server is
      Port : Port_Type;
      Priority : System.Priority;
      Address : Sock_Addr_Type;
      Server : Socket_Type;
      Socket : Socket_Type;
   begin
      Initialize;
      accept Start (Using_Port : GNAT.Sockets.Port_Type;
                   At_Priority : System.Priority) do
         Port := Using_Port;
         Priority := At_Priority;
      end Start;
      Ada.Dynamic_Priorities.Set_Priority (Priority);
      Address.Addr := Any_Inet_Addr;
      Address.Port := Port;
      Create_Socket (Server);
      Set_Socket_Option
        (Server,
         Socket_Level,
         (Reuse_Address, True));
      Bind_Socket (Server, Address);
      Listen_Socket (Server);
      loop
         Accept_Socket (Server, Socket, Address);
--           Put_Line ("connected socket: " & Image (Socket));
--           Put_Line ("Address: " & Image (Get_Peer_Name (Socket)));
         declare
            Request : aliased HTTP.Request;
         begin
            HTTP.Initialize (Request, From => Socket);
--              Put_Line ("URL |"
--                      & HTTP.Get_URL (Request)
--                      & "|");
            begin
               HTTP.Respond (HTTP.Find (Request'Unchecked_Access),
                             To => Socket);
            exception
               when E : others =>
                  Put_Line ("failed in immediate read/respond, "
                            & Exception_Information (E));
               begin
                  HTTP.Respond
                    (HTTP.Exception_Response (E, Request'Unchecked_Access),
                     To => Socket);
               end;
            end;
            Close_Socket (Socket);
         exception
            when E : others =>
               Put_Line ("failed in outer read/respond, "
                           & Exception_Information (E));
               begin
                  Close_Socket (Socket);
               exception
                  when others => null;
               end;
         end;
      end loop;
   exception
      when E : others =>
         Put_Line ("failed in outer, " & Exception_Information (E));
         Close_Socket (Server);
   end Server;


   procedure Serve
     (Using_Port : GNAT.Sockets.Port_Type;
      At_Priority : System.Priority := System.Default_Priority) is
      New_Server : constant Server_P := new Server;
   begin
      New_Server.Start (Using_Port, At_Priority);
   end Serve;


end EWS.Server;
