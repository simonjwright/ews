%  EWS is free software; you can redistribute it and/or
%  modify it under terms of the GNU General Public License as
%  published by the Free Software Foundation; either version 2, or (at
%  your option) any later version. This package is distributed in the
%  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
%  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
%  PURPOSE. See the GNU General Public License for more details. You
%  should have received a copy of the GNU General Public License
%  distributed with this package; see file COPYING.  If not, write to
%  the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
%  MA 02111-1307, USA.
%
% Copyright (C) 2013-2022, Simon Wright <simon@pushface.org>
% All rights reserved.

\documentclass{report}

\usepackage{color}
\definecolor{linkcolor}{rgb}{0, 0, 0.7}

\usepackage[
backref,
raiselinks,
pdfhighlight=/O,
pagebackref,
hyperfigures,
breaklinks,
colorlinks,
pdfpagemode=UseNone,
pdfstartview=FitBH,
linkcolor={linkcolor},
anchorcolor={linkcolor},
citecolor={linkcolor},
filecolor={linkcolor},
menucolor={linkcolor},
urlcolor={linkcolor}
]{hyperref}

\usepackage{parskip}
\setlength{\oddsidemargin}{0in}
\setlength{\evensidemargin}{0in}
\setlength{\topmargin}{0in}
\addtolength{\topmargin}{-\headheight}
\addtolength{\topmargin}{-\headsep}
\setlength{\textheight}{8.9in}
\setlength{\textwidth}{6.5in}
\setlength{\marginparwidth}{0.5in}

\usepackage{graphicx}

\title{Embedded Web Server}
\date{9.viii.2022}
\author{Simon Wright
\\ \sl simon@@pushface.org}

\begin{document}
\pagenumbering{roman}
\maketitle
\tableofcontents
\listoffigures

\chapter{Introduction}
\pagenumbering{arabic}

\newcommand{\EWS}{\href{https://github.com/simonjwright/ews}{EWS}\ }

This document describes the
\href{https://github.com/simonjwright/ews}{Embedded Web Server}
(EWS) in the form of a demonstration program.

EWS is intended for small, limited embedded systems (for example, ones
with no file system).

It provides a program (\verb|ews_generator|) which converts a
directory structure containing a set of web pages into an Ada data
structure to be compiled with the EWS library and your application and
served at run time.

As well as static web pages, EWS supports dynamic interactions, where
the client makes a request and the server responds. This can be used
to provide a complete new page, constructed on the fly by the server,
or (more interestingly) in an
\href{http://en.wikipedia.org/wiki/Ajax_(programming)}{AJAX} style,
where the server takes some action and the response is interpreted by
\href{http://en.wikipedia.org/wiki/Javascript}{JavaScript} in the
client.

\begin{figure}[ht]
\centering
\includegraphics[width=0.5\columnwidth]{ews.png}
\caption{The dynamic part of the example page}
\label{fig:ews}
\end{figure}

Figure \ref{fig:ews} shows the part of the example page which
demonstrates AJAX-style interactions.

These interactions are supported by EWS's \verb|HttpInteraction.js|.

\chapter{Ada main program}

This chapter describes the Ada main program and the
\href{https://docs.adacore.com/live/wave/gprbuild/html/gprbuild_ug/gprbuild_ug.html}{GNAT Project}
used to build it.

\section{Ada code}

The main program (\verb|EWS_Demo|).

@O ews_demo.adb @{@%
@< Ada licence header @>

@< Main program standard context: Ada @>

@< Main program generated context: Ada @>

procedure EWS_Demo is

   use EWS;

   @< Specs of dynamic pages: Ada @>

   @< Global data: Ada @>

   @< Bodies of dynamic pages: Ada @>

   Verbose : Boolean := False;

begin

   begin
      loop
         case GNAT.Command_Line.Getopt ("v") is
            when 'v' =>
               Verbose := True;
            when ASCII.NUL =>
               exit;
            when others =>
               null;  -- never taken
         end case;
      end loop;
   exception
      when GNAT.Command_Line.Invalid_Switch =>
         Put_Line (Standard_Error,
                   "invalid switch -" & GNAT.Command_Line.Full_Switch);
         return;
   end;

   @< Register dynamic pages: Ada @>

   Put_Line ("Connect to ews_demo using e.g. http://localhost:8080");

   Server.Serve (Using_Port => 8080,
                 With_Stack => 40_000,
                 Tracing => Verbose);

   delay 1_000_000.0;

end EWS_Demo;
@| EWS_Demo @}

This is the context required for the main program, with the exception
of that for the code generated from the site file tree.

@d Main program standard context: Ada @{@%
with Ada.Calendar;
with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;
with Ada.Text_IO; use Ada.Text_IO;
with EWS.Dynamic;
with EWS.HTTP;
with EWS.Server;
with EWS.Types;
with GNAT.Calendar.Time_IO;
with GNAT.Command_Line;
@|@}

This is the output of the program \verb|ews_generator|, which
converts a file tree into static Ada source code.

@d Main program generated context: Ada @{@%
with EWS_Htdocs;
@|@}

\section{GNAT Project}

@o ews_demo.gpr @{@%
with "../ews";
with "xmlada";
project EWS_Demo is

   for Main use ("ews_demo.adb");
   for Exec_Dir use ".";
   for Source_Dirs use (".");
   for Object_Dir use ".build";
   for Create_Missing_Dirs use "true";

   package Builder is
      for Default_Switches ("ada") use ("-g");
   end Builder;

   package Compiler is
      for Default_Switches ("ada") use
        (
         "-O2",
         "-gnatqQafoy"
        );
   end Compiler;

   package Binder is
      for Default_Switches ("ada") use ("-E");
   end Binder;

end EWS_Demo;
@|@}

\chapter{Copyright and Licencing}

\EWS itself is licenced under the
\href{http://www.gnu.org/licenses/gpl.html}{GPL version 3}; the code
that forms part of the run time (Ada and JavaScript) has the
additional permissions granted by the
\href{http://www.gnu.org/licenses/gcc-exception.html}{GCC Runtime
Library Exception version 3.1}.

The demonstration code is released without restriction.

@d Copyright @{@%
Copyright 2013-2022, Simon Wright <simon@@pushface.org>@%
@}

In Ada,
@d Ada licence header @{@%
--  @< Copyright @>
--
--  This unit is free software; you can redistribute it and/or modify
--  it as you wish. This unit is distributed in the hope that it will
--  be useful, but WITHOUT ANY WARRANTY; without even the implied
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
@|@}

In JavaScript,
@d JavaScript licence header @{@%
/*
 * @< Copyright @>
 *
 * This unit is free software; you can redistribute it and/or modify
 * it as you wish. This unit is distributed in the hope that it will
 * be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 */
@|@}

In HTML,
@d HTML licence header @{@%
<!--
  @< Copyright @>

  This unit is free software; you can redistribute it and/or modify
  it as you wish. This unit is distributed in the hope that it will
  be useful, but WITHOUT ANY WARRANTY; without even the implied
  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  -->
@|@}

\chapter{Cyclic updating and select/options}

This part of the demonstration, shown in Figure \ref{fig:date-time},
shows cyclic updating of a portion of the web page (the time, as seen
at the server) and the ability to change the format of the display
using the HTML `select' and `options'.

\begin{figure}[ht]
\centering
\includegraphics[width=0.5\columnwidth]{date-time.png}
\caption{Displaying the current time, and controlling the format}
\label{fig:date-time}
\end{figure}

@d Cyclic updating and select/options: HTML @{@%
<tr>
  @< Display the current time: HTML @>
</tr>
<tr>
  @< Change the time display format: HTML @>
</tr>
@|@}

\section{Displaying the current time}

The HTML displays the current time in a span with id
\verb|timeDisplay| inside a two-column table data cell.

@d Display the current time: HTML @{@%
<td colspan="2">
  Displaying the current date/time
  (<span id="timeDisplay" style="background:yellow">here</span>)
  as seen at the server.
</td>
@| timeDisplay @}

The corresponding JavaScript runs a \verb|CyclicHttpInteraction| with
a 1-second repetiton rate to the URL \verb|ajaxTime|. The response is
expected to be a \verb|text/plain| string, which is pasted into the
document at the element with id \verb|timeDisplay|.

@d Cyclic interactions: JavaScript @{@%
var timeRequest = new CyclicHttpInteraction
  ("ajaxTime",
   function (r) {
    document.getElementById("timeDisplay").innerHTML = r.responseText;
   },
   1000);
@| timeRequest @}

The cyclic interaction needs to be started when the page is loaded.

@d Start getting cyclic data: JavaScript @{@%
timeRequest.start();
@|@}

The Ada code which receives the cyclic \verb|ajaxTime| request is in
the function \verb|AJAX_Time|.

@d Specs of dynamic pages: Ada @{@%
function AJAX_Time
  (From_Request : HTTP.Request_P)
  return Dynamic.Dynamic_Response'Class;
@|AJAX_Time@}

The function is registered with the server, to be called to respond to
the URL \verb|ajaxTime|. We need to use GNAT's implementation-defined
attribute \verb|'Unrestricted_Access| because \verb|AJAX_Time| isn't
declared at library level; this is unlikely to be a problem in a real
program.

@d Register dynamic pages: Ada @{@%
Dynamic.Register (AJAX_Time'Unrestricted_Access, "/ajaxTime");
@|ajaxTime@}

\verb|AJAX_Time| uses global data to store the time format that is
required.

@d Global data: Ada @{@%
type Date_Format is (ISO, US, European, Locale);
Current_Date_Format : Date_Format := ISO;
@|Date_Format Current_Date_Format@}

The implementation of \verb|AJAX_Time| returns the current date/time
as plain text in the format selected in \verb|Current_Date_Format|.

@D Bodies of dynamic pages: Ada @{@%
function AJAX_Time
  (From_Request : HTTP.Request_P) return Dynamic.Dynamic_Response'Class is
   Result : Dynamic.Dynamic_Response (From_Request);
   function Format return GNAT.Calendar.Time_IO.Picture_String;
   function Format return GNAT.Calendar.Time_IO.Picture_String is
   begin
      case Current_Date_Format is
         when ISO => return GNAT.Calendar.Time_IO.ISO_Date;
         when US => return GNAT.Calendar.Time_IO.US_Date;
         when European => return GNAT.Calendar.Time_IO.European_Date;
         when Locale => return "%c";
      end case;
   end Format;
begin
   Result.Set_Content_Type (To => Types.Plain);
   Result.Set_Content
     (GNAT.Calendar.Time_IO.Image (Ada.Calendar.Clock, Format));
   return Result;
end AJAX_Time;
@|AJAX_Time@}

\section{Changing the time display format}

The choice of time format is implemented in HTML in a table data cell
containing a form \verb|fTimeFormat| containing a drop-down list, with
the value associated with each option being that of the corresponding
value of the Ada \verb|Date_Format| (this makes it easy for the Ada
code to determine which value has been sent, using
\verb|Date_Format'Value|).

@d Change the time display format: HTML @{@%
<td>
  Using select/options
</td>
<td>
  <form method="POST" name="fTimeFormat" id="fTimeFormat">
    <select name="format">
      <option value="iso" selected="true">ISO</option>
      <option value="us">US</option>
      <option value="european">European</option>
      <option value="locale">Local</option>
    </select>
  </form>
</td>
@|fTimeFormat@}

The form \verb|fTimeFormat| nominally \verb|POST|s the request, but
this is overridden using \verb|postChange|. The selected option is
sent as a query in the form \verb|timeformat=iso|,
\verb|timeFormat=us| etc.

@d Set up to send time format: JavaScript @{@%
document.fTimeFormat.format.onchange = function () {
  for (var o = document.fTimeFormat.format.options, i = 0;
       i < o.length;
       i++) {
    if (o[i].selected) {
      postChange.start("timeFormat=" +  o[i].value);
      break;
    }
  }
};
@|@}

In \verb|AJAX_Change|, check whether it has been called to change the
time format; a query \verb|foo=bar| can be retrieved from the
\verb|Request| as the property \verb|"foo"| with value \verb|"bar"|
(if the property isn't present in the request, the empty string is
returned).

@d Checks for changed properties: Ada @{@%
declare
   Property : constant String
     := EWS.HTTP.Get_Property ("timeFormat", From_Request.all);
begin
   if Property /= "" then
      Put_Line ("saw timeFormat=" & Property);
      Current_Date_Format := Date_Format'Value (Property);
   end if;
end;
@|@}

\chapter{Radio buttons}

Note, with radio buttons the \verb|value| identifies which radio
button has been pressed, and does not change; it's the \verb|checked|
field which changes, and only one can be \verb|true| at a time.

\begin{figure}[ht]
\centering
\includegraphics[width=0.5\columnwidth]{radio-buttons.png}
\caption{Using radio buttons}
\label{fig:radio-buttons}
\end{figure}

Figure \ref{fig:radio-buttons} shows the part of the example that
relates to radio buttons. There are two lights, Forward and Aft, each
of which can show Red or Blue. The HTML is implemented in a form with

@D Radio buttons: HTML @{@%
<tr>
  <td>
    Using radio buttons
  </td>
  <td>
    <form method="PUT" name="lights" id="lights">
      <table border="1">
        <tr>
          <td id="forward-light">Forward Light</td>
          <td>
            <input
               type="radio"
               name="forward"
               value="red"
               checked="true">Red</input>
          </td>
          <td>
            <input
               type="radio"
               name="forward"
               value="blue">Blue</input>
          </td>
        </tr>
        <tr>
          <td id="aft-light">Aft Light</td>
          <td>
            <input
               type="radio"
               name="aft"
               value="red"
               checked="true">Red</input>
          </td>
          <td>
            <input
               type="radio"
               name="aft"
               value="blue">Blue</input>
          </td>
        </tr>
      </table>
    </form>
  </td>
</tr>
@|lights forward-light aft-light@}

The radio button scripts have to be set up when the page is loaded.

@d Set up the radio buttons: JavaScript @{@%
@< "Set up radio buttons" utility: JavaScript @%
    @( document.lights.forward @, "forward-light" @) @>
@< "Set up radio buttons" utility: JavaScript @%
    @( document.lights.aft @, "aft-light" @) @>
@|@}

The Ada code which receives the \verb|forward-| and
\verb|backward-light| property changes updates global data.

@d Global data: Ada @{@%
type Light_State is (Red, Blue);
Forward_Light : Light_State := Red;
Aft_Light : Light_State := Red;
@|Light_State Forward_Light Aft_Light@}

@d Checks for changed properties: Ada @{@%
declare
   Property : constant String
     := EWS.HTTP.Get_Property ("forward-light", From_Request.all);
begin
   if Property /= "" then
      Put_Line ("saw forward-light=" & Property);
      Forward_Light := Light_State'Value (Property);
   end if;
end;
declare
   Property : constant String
     := EWS.HTTP.Get_Property ("aft-light", From_Request.all);
begin
   if Property /= "" then
      Put_Line ("saw aft-light=" & Property);
      Aft_Light := Light_State'Value (Property);
   end if;
end;
@|@}

Because the server can be accessed by more than one web client, and
all the other clients need to show changes, the current light state is
retrieved every second via a \verb|CyclicHttpInteraction| to the URL
\verb|lightState.xml|. The response is expected to be XML:

\begin{verbatim}
<lights>
  <forward-light>lmp</forward-light>
  <aft-light>lmp</aft-light>
</lights>
\end{verbatim}
where \verb|lmp| specifies a colour (will be \verb|red| or \verb|blue|).

@d Cyclic interactions: JavaScript @{@%
var lightStateRequest = new CyclicHttpInteraction
  ("lightState.xml",
   function (r) {
     var xml = r.responseXML;
     document.getElementById("forward-light").style.color =
       xml.getElementsByTagName("forward-light")[0].firstChild.nodeValue;
     document.getElementById("aft-light").style.color =
       xml.getElementsByTagName("aft-light")[0].firstChild.nodeValue;
   },
   1000);
@|lightStateRequest@}

The cyclic interaction needs to be started when the page is loaded.

@d Start getting cyclic data: JavaScript @{@%
lightStateRequest.start();
@|@}

The Ada code which receives the cyclic \verb|lightState.xml| request
is in the function \verb|AJAX_Light_State|.

@d Specs of dynamic pages: Ada @{@%
function AJAX_Light_State
  (From_Request : HTTP.Request_P)
  return Dynamic.Dynamic_Response'Class;
@|AJAX_Light_State@}

The function is registered with the server, to be called to respond to
the URL \verb|lightState.xml|.

@d Register dynamic pages: Ada @{@%
Dynamic.Register (AJAX_Light_State'Unrestricted_Access, "/lightState.xml");
@|lightState.xml@}

@D Bodies of dynamic pages: Ada @{@%
function AJAX_Light_State
  (From_Request : HTTP.Request_P)
  return Dynamic.Dynamic_Response'Class is
   Result : Dynamic.Dynamic_Response (From_Request);
begin
   Result.Set_Content_Type (To => Types.XML);
   Result.Append ("<lights>");
   Result.Append_Element
     ("forward-light",
      Ada.Strings.Fixed.Translate
        (Forward_Light'Image,
         Ada.Strings.Maps.Constants.Lower_Case_Map));
   Result.Append_Element
     ("aft-light",
      Ada.Strings.Fixed.Translate
        (Aft_Light'Image,
         Ada.Strings.Maps.Constants.Lower_Case_Map));
   Result.Append ("</lights>");
   return Result;
end AJAX_Light_State;
@|AJAX_Light_State@}

\chapter{Checkboxes}

\begin{figure}[ht]
\centering
\includegraphics[width=0.5\columnwidth]{checkboxes.png}
\caption{Using checkboxes}
\label{fig:checkboxes}
\end{figure}

Figure \ref{fig:checkboxes} shows the part of the example that relates
to checkboxes. There are two Lamps, Port and Starboard, which are
separately switched.

@D Checkboxes: HTML @{@%
<tr>
  <td>
    Using checkboxes
  </td>
  <td>
    <form method="PUT" name="lamps" id="lamps">
      <table border="1">
        <tr>
          <td>Port Lamp</td>
          <td>
            <input
               type="checkbox"
               name="lamp"
               value="port"/>
          </td>
        </tr>
        <tr>
          <td>Starboard Lamp</td>
          <td>
            <input
               type="checkbox"
               name="lamp"
               value="starboard"/>
          </td>
        </tr>
      </table>
    </form>
  </td>
</tr>
@|lamps lamp@}

The \verb|onclick| action is a function whose source is, for example,

\begin{verbatim}
postChange.start('lamp=0&value=port&checked='+document.lamps.lamp[0].checked);
\end{verbatim}

Note that this sends the clicked checkbox's index
(\verb|document.lamps.lamp[0]| is the first) as well as the
\verb|value| (corresponding to the internal name of the box); the
receiving Ada code presently uses the index, though it would obviously
be better to use the value.

@d Set up the checkboxes: JavaScript @{@%
for (var c = document.lamps.lamp, i = 0; i < c.length; i++) {
  c[i].onclick = new Function(
    "postChange.start('lamp=" + i
      + "&value=" + document.lamps.lamp[i].value
      + "&checked=' + document.lamps.lamp[" + i + "].checked);"
  );
}
@|@}

The Ada code which receives the \verb|lamp| property changes updates
global data.

@d Global data: Ada @{@%
Lamps : array (0 .. 1) of Boolean := (others => True);
@|Lamps@}

@d Checks for changed properties: Ada @{@%
declare
   Lamp : constant String
     := EWS.HTTP.Get_Property ("lamp", From_Request.all);
begin
   if Lamp /= "" then
      declare
         Checked : constant String
           := EWS.HTTP.Get_Property ("checked", From_Request.all);
         Value : constant String
           := EWS.HTTP.Get_Property ("value", From_Request.all);
      begin
         Put_Line ("saw lamp=" & Lamp
                   & " value=" & Value
                   & " checked=" & Checked);
         Lamps (Natural'Value (Lamp)) := Boolean'Value (Checked);
      end;
   end if;
end;
@|@}

\chapter{File upload}

Figure \ref{fig:file-upload} shows the file upload dialog. Figure
\ref{fig:upload-done} shows the alert box displayed when the upload is
complete.

\begin{figure}[ht]
\centering
\includegraphics[width=0.5\columnwidth]{file-upload.png}
\caption{The file upload dialog}
\label{fig:file-upload}
\end{figure}

\begin{figure}[ht]
\centering
\includegraphics[width=0.5\columnwidth]{uploaded.png}
\caption{Upload completed}
\label{fig:upload-done}
\end{figure}

The file upload dialog is in a form, in a table cell. The form
contains two elements: a file selector (\verb|datafile|) and a submit
button.

When the form is submitted, the content of the selected file is sent
as a multipart attachment to the URL \verb|/fileInput| (the form's
\verb|action|).

The other components of this example program use explicit JavaScript
methods to send the data to the server. In the case of a file upload,
the protocol is complex enough that it's best left to the
browser. When the browser has submitted the request, it expects to get
a new page in response; we don't want that, so we use the form's
\verb|target| attribute to direct the response to an invisible frame
\verb|iFrame| so that the current page remains displayed.

@d File upload: HTML @{@%
<tr>
  <td>
    File upload
  </td>
  <td>
    <form method="POST"
          enctype="multipart/form-data"
          name="fileInput"
          action="fileInput"
          target="iFrame">
      <input type="file" name="datafile" size="128">
      <input type="submit" name="send" value="Send">
    </form>
  </td>
</tr>
@|@}

@d File upload's target iFrame: HTML @{@%
<iframe name="iFrame" id="iFrame" src="about:blank" width="0" height="0">
</iframe>
@|iFrame@}

The Ada code which receives the \verb|fileInput| request is in the
function \verb|File_Input|.

@d Specs of dynamic pages: Ada @{@%
function File_Input
  (From_Request : HTTP.Request_P)
  return Dynamic.Dynamic_Response'Class;
@|File_Input@}

The function is registered with the server, to be called to respond to
the URL \verb|fileInput|.

@d Register dynamic pages: Ada @{@%
Dynamic.Register (File_Input'Unrestricted_Access, "/fileInput");
@|fileInput@}

The response generated is a page containing an alert box
(\ref{fig:upload-done}), which is displayed even though the
\verb|iFrame| swallows the HTML content.

@D Bodies of dynamic pages: Ada @{@%
function File_Input
  (From_Request : HTTP.Request_P) return Dynamic.Dynamic_Response'Class is
   @< \verb|Upload_Result|, calculates file input result: Ada @>
   C : HTTP.Cursor;
   Lines : Natural := 0;
   Line : String (1 .. 1024);
   Last : Natural;
   Attachments : constant HTTP.Attachments
     := HTTP.Get_Attachments (From_Request.all);
   Content : constant HTTP.Contents
     := HTTP.Get_Content (From => Attachments);
begin
   Put_Line ("saw fileInput with attachment length"
             & Content'Length'Image);
   if Content'Length /= 0 then
      begin
         case HTTP.Get_Content_Kind (Content) is
            when HTTP.Text =>
               HTTP.Open (C, Content);
               while not HTTP.End_Of_File (C) loop
                  Lines := Lines + 1;
                  Put (Lines'Image & ": ");
                  HTTP.Get_Line (C, Line, Last);
                  Put_Line (Line (Line'First .. Last));
               end loop;
               HTTP.Close (C);
               return Upload_Result
                 ("Upload complete," & Lines'Image & " lines.");
            when others =>
               return Upload_Result
                 ("Upload complete," & Content'Length'Image & " bytes.");
         end case;
      exception
         when E : others =>
            begin
               HTTP.Close (C);
            exception
               when others => null;
            end;
            return Upload_Result
              ("Upload failed: " & Ada.Exceptions.Exception_Message (E));
      end;
   else
      return Upload_Result ("Upload complete, zero bytes.");
   end if;
end File_Input;
@|File_Input@}

@d \verb|Upload_Result|, calculates file input result: Ada @{@%
function Upload_Result (Message : String)
  return Dynamic.Dynamic_Response'Class;
@|Upload_Result@}

If the \verb|Message| to be returned contains multiple lines, they
have to be translated to the \verb|\n| that the JavaScript
\verb|alert()| function expects.

@d \verb|Upload_Result|, calculates file input result: Ada @{@%
function Upload_Result (Message : String)
  return Dynamic.Dynamic_Response'Class is
   Result : Dynamic.Dynamic_Response (From_Request);
begin
   Result.Set_Content_Type (To => Types.HTML);
   Result.Append ("<body onload=""alert('");
   for C in Message'Range loop
      case Message (C) is
         when ASCII.CR | ASCII.NUL => null;
         when ASCII.LF => Result.Append ("\n");
         when others =>
            Result.Append (String'(1 => Message (C)));
      end case;
   end loop;
   Result.Append ("')"">");
   return Result;
end Upload_Result;@%
@|Upload_Result@}

\chapter{Retrieving the initial state}

When a new client connects to the server, it must retrieve the current
state and set the page elements accordingly.

The server responds to the URL \verb|state.xml| with the current state
in XML format,
\begin{verbatim}
<state>
  <time-format>fmt</time-format>
  <forward-light>lmp</forward-light>
  <aft-light>lmp</aft-light>
  <lamp>bool</lamp>
  <lamp>bool</lamp>
</state>
\end{verbatim}

where

\verb|fmt| can be \verb|iso|, \verb|us|, \verb|european| or \verb|locale|,\\
\verb|lmp| can be \verb|red| or \verb|blue|, and\\
\verb|bool| can be \verb|false| or \verb|true|.

and the first \verb|lamp| element is for the starboard lamp and the
second is for the port lamp.

@D Retrieving the initial state: JavaScript @{@%
var stateRequest = new OneshotHttpInteraction
  ("state.xml",
   null,
   function (r) {
     var x = r.responseXML;
     var value = x.getElementsByTagName("time-format")[0].firstChild.nodeValue;
     for (var o = document.fTimeFormat.format.options, i = 0;
          i < o.length;
          i++) {
       o[i].selected = (o[i].value == value);
     }
     value = x.getElementsByTagName("forward-light")[0].firstChild.nodeValue;
     for (var o = document.lights.forward, i = 0;
          i < o.length;
          i++) {
       o[i].checked = (o[i].value == value);
     }
     value = x.getElementsByTagName("aft-light")[0].firstChild.nodeValue;
     for (var o = document.lights.aft, i = 0;
          i < o.length;
          i++) {
       o[i].checked = (o[i].value == value);
     }
     var lamps = x.getElementsByTagName("lamp");
     for (var c = document.lamps.lamp, i = 0; i < c.length; i++) {
       c[i].checked = lamps[i].firstChild.nodeValue == "true";
     }
   });
@|stateRequest@}

When the page is opened, request the initial state.

@d Request the initial state: JavaScript @{@%
stateRequest.start();
@|@}

The Ada code which receives the \verb|state.xml| request is in the
function \verb|AJAX_Status|.

@d Specs of dynamic pages: Ada @{@%
function AJAX_Status
  (From_Request : HTTP.Request_P)
  return Dynamic.Dynamic_Response'Class;
@|AJAX_Status@}

The function is registered with the server, to be called to respond to
the URL \verb|state.xml|.

@d Register dynamic pages: Ada @{@%
Dynamic.Register (AJAX_Status'Unrestricted_Access, "/state.xml");
@|state.xml@}

@D Bodies of dynamic pages: Ada @{@%
function AJAX_Status
  (From_Request : HTTP.Request_P)
  return Dynamic.Dynamic_Response'Class is
   Result : Dynamic.Dynamic_Response (From_Request);
begin
   Result.Set_Content_Type (To => Types.XML);
   Result.Append ("<state>");
   Result.Append_Element
     ("time-format",
      Ada.Strings.Fixed.Translate
        (Current_Date_Format'Image,
         Ada.Strings.Maps.Constants.Lower_Case_Map));
   Result.Append_Element
     ("forward-light",
      Ada.Strings.Fixed.Translate
        (Forward_Light'Image,
         Ada.Strings.Maps.Constants.Lower_Case_Map));
   Result.Append_Element
     ("aft-light",
      Ada.Strings.Fixed.Translate
        (Aft_Light'Image,
         Ada.Strings.Maps.Constants.Lower_Case_Map));
   for L in Lamps'Range loop
      Result.Append_Element
        ("lamp",
         Ada.Strings.Fixed.Translate
           (Lamps (L)'Image,
            Ada.Strings.Maps.Constants.Lower_Case_Map));
   end loop;
   Result.Append ("</state>");
   return Result;
end AJAX_Status;
@|AJAX_Status@}

\chapter{Utilities}

\section{Notifying the server of changes}

\verb|postChange| is a one-shot interaction; it sends a request to the
URL \verb|ajaxChange|. If \verb|postChange.start()| is called with a
parameter (for example, \verb|"foo=bar"|, the parameter is sent to the
URL as a query. No specific response is expected.

@d Generalised change action request: JavaScript @{@%
var postChange = new OneshotHttpInteraction
  ("ajaxChange",
   null,
   function (r) { });
@|postChange@}

The Ada code which receives the \verb|ajaxChange| request is in the
function \verb|Ajax_Change|.

@d Specs of dynamic pages: Ada @{@%
function AJAX_Change
  (From_Request : HTTP.Request_P)
  return Dynamic.Dynamic_Response'Class;
@|AJAX_Change@}

The function is registered with the server, to be called to respond to
the URL \verb|ajaxChange|.

@d Register dynamic pages: Ada @{@%
Dynamic.Register (AJAX_Change'Unrestricted_Access, "/ajaxChange");
@|ajaxChange@}

@D Bodies of dynamic pages: Ada @{@%
function AJAX_Change
  (From_Request : HTTP.Request_P)
  return Dynamic.Dynamic_Response'Class is
   Result : Dynamic.Dynamic_Response (From_Request);
begin
   Put_Line ("AJAX_Change called.");
   @< Checks for changed properties: Ada @>
   Result.Set_Content_Type (To => Types.Plain);
   Result.Set_Content ("OK");
   return Result;
end AJAX_Change;
@|AJAX_Change@}

\section{Set up radio buttons}

\verb|Set up radio buttons utility| is a \verb|nuweb| parameterised
fragment. There's no indication in a parameterised fragment's name
that it is parameterised; when invoked with parameters, occurrences of
\verb|@@n| are replaced by the \verb|n|'th parameter.

The first parameter (\verb|@@1|) is the name of the buttons to be set
up (they all have the same name): e.g. \verb|document.formName.buttonName|.

The second parameter (\verb|@@2|) is the property name that is passed
to \verb|postChange|.

When one of the buttons is clicked, a one-shot \verb|ajaxChange|
interaction is invoked, posting the query \verb|property=value| where
\verb|property| is the name passed in the second parameter and
\verb|value| is the \verb|value| attribute of the button.

@d "Set up radio buttons" utility: JavaScript @{@%
for (var j = 0; j < @1.length; j++) {
  @1[j].onclick = new Function(
     "postChange.start('" + @2 + "=" + @1[j].value + "');");
};@%
@|@}

\chapter{HTML pages}

@o ajax.html @{@%
@<  HTML licence header @>
<html>
<head>
@< HTML header @>
</head>
<body bgcolor="white">
@< Page heading @>
@< Introductory material @>
<p><hr>
@< The demonstrations @>
@< Author link @>
</body>
</html>
@|@}

@d HTML header @{@%
<title>EWS: AJAX demonstration</title>

<!-- NB,for Internet Explorer you mustn't use the empty-element
     syntax. For Safari, you have to close the element. -->
<script type="text/javascript"src="HttpInteraction.js"></script>
<script type="text/javascript"src="ajax.js"></script>

<style type="text/css">
  div#demos table { margin : 0.2em 1em;
                    font-size : 100%;
                    border-collapse : collapse; }
  div#demos th,td { padding : 0.2em; }
</style>
@|@}

@d Page heading @{@%
<table width="100%">
<tr>
<td><h1>Embedded Web Server: AJAX demonstration</h1></td>
<td align="right">
<a href="https://github.com">
<img
src="GitHub_Logo.png"
width="88" height="31" border="0" alt="Github Logo" />
</a>
</td>
</tr>
</table>
@|@}

@d Introductory material @{@%
<p>EWS is a web server construction kit, designed for embedded
applications using the GNAT Ada compiler.

<p>The project is hosted on <a
href="https://github.com/simonjwright/ews">Github</a>.

<p>EWS comes with a demonstration of its facilities. The available
facilities are described in <a href="ews.pdf">this document</a>, which
also acts as the source code for the demonstration using the <a
href="http://www.literateprogramming.com/">Literate Programming</a>
facilities of <a
href="https://github.com/simonjwright/nuweb.py">nuweb.py</a>.
@|@}

@d The demonstrations @{@%
<p>Below are demonstrations of
various <a href="http://www.amazon.co.uk/exec/obidos/ASIN/0471777781/qid%3D1146719450/203-6928631-0011916">AJAX</a>
technologies:

<div id="demos" align="center">
  <table border="1">
    @< Cyclic updating and select/options: HTML @>
    @< Radio buttons: HTML @>
    @< Checkboxes: HTML @>
    @< File upload: HTML @>
  </table>
</div>
@< File upload's target iFrame: HTML @>
@|@}

@d Author link @{@%
<hr>
<i>
<address>
<a href="mailto:simon@@pushface.org">Simon Wright</a>
</address>
</i>
@|@}

\chapter{JavaScript}

This script, loaded by \verb|ajax.html|, relies on the utility
\verb|HttpInteraction.js| having been already loaded by the page.

@O ajax.js @{@%
@< JavaScript licence header @>

@< Retrieving the initial state: JavaScript @>

@< Cyclic interactions: JavaScript @>

@< Generalised change action request: JavaScript @>

/**
 * Assign event handlers and begin fetching.
 */
window.onload = function () {

  @< Request the initial state: JavaScript @>
  @< Start getting cyclic data: JavaScript @>
  @< Set up to send time format: JavaScript @>
  @< Set up the radio buttons: JavaScript @>
  @< Set up the checkboxes: JavaScript @>
};
@|@}

\appendix

\chapter{About this document}

This document is prepared using
\href{https://sourceforge.net/projects/nuweb/}{nuweb}, a
language-agnostic \href{http://www.literateprogramming.com/}{Literate Programming} tool. The actual variant used
is
\href{https://github.com/simonjwright/nuweb.py}{nuweb.py}.

\chapter{Index} \label{index}

\section{Files}

@f

\section{Macros}

@m

\section{Definitions}

@u

\end{document}
