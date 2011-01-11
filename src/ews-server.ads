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

with GNAT.Sockets;
with System;

package EWS.Server is

   pragma Elaborate_Body;

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
