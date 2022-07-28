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

with GNAT.Sockets;
with System;

package EWS.Server with Elaborate_Body is

   type Error_Level is (Trace, Info, Error);

   type Logger is access procedure (Message : String; Level : Error_Level);

   procedure Serve (Using_Port : GNAT.Sockets.Port_Type;
                    At_Priority : System.Priority := System.Default_Priority;
                    With_Stack : Positive := 20_000;
                    Logging_Via : Logger := null;
                    Tracing : Boolean := False);
   --  At_Priority and With_Stack refer to the settings to be used for
   --  the embedded task that deals with client requests.
   --  Logging_Via tells where to send log messages (the default sends
   --  output to standard error).
   --  Tracing tells the server to report requests.

end EWS.Server;
