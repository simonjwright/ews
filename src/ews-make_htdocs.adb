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

with Ada.Command_Line;
with Ada.Streams.Stream_IO; use Ada.Streams; use Ada.Streams.Stream_IO;
with Ada.Text_IO; use Ada.Text_IO;
with EWS.Types; use EWS.Types;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with GNAT.Regpat; use GNAT.Regpat;

procedure EWS.Make_Htdocs is

   procedure Scan_Directory (Named : Dir_Name_Str);
   procedure Save_File (Named : String);
   procedure Output (Base_Dir : String);
   procedure Output_Contents (Of_File : String);

   procedure Scan_Directory (Named : Dir_Name_Str) is
      Wd : Dir_Type;
   begin
      Put_Line (Standard_Error, "scan_directory(" & Named & ")");
      Open (Dir => Wd,
            Dir_Name => Named);
      declare
         Str : String (1 .. 1024);
         Last : Natural;
      begin
         loop
            Read (Dir => Wd,
                  Str => Str,
                  Last => Last);
            exit when Last = 0;
            if Str (1 .. Last) /= "." and Str (1 .. Last) /= ".." then
               if Is_Directory (Named & Str (1 .. Last)) then
                  Scan_Directory
                    (Named & Str (1 .. Last) & Directory_Separator);
               else
                  Save_File (Named & Str (1 .. Last));
               end if;
            end if;
         end loop;
      end;
      Close (Dir => Wd);
   end Scan_Directory;

   CSS_File : constant Pattern_Matcher := Compile ("\.css$");
   GIF_File : constant Pattern_Matcher := Compile ("\.gif$");
   HTML_File : constant Pattern_Matcher := Compile ("\.(html|htm)$");
   ICO_File : constant Pattern_Matcher := Compile ("\.ico$");
   JPEG_File : constant Pattern_Matcher := Compile ("\.(jpeg|jpg)$");
   JavaScript_File : constant Pattern_Matcher := Compile ("\.js$");
   Java_File : constant Pattern_Matcher := Compile ("\.(class|jar)$");
   PNG_File : constant Pattern_Matcher := Compile ("\.png$");
   XML_File : constant Pattern_Matcher := Compile ("\.xml$");
   XSL_File : constant Pattern_Matcher := Compile ("\.xsl$");

   type String_P is access constant String;
   subtype Content_Type is Types.Format;
   type File_Info;
   type File_Info_P is access File_Info;
   type File_Info is record
      Name : String_P;
      Kind : Content_Type;
      Next : File_Info_P;
   end record;

   First_File, Last_File : File_Info_P;

   procedure Save_File (Named : String) is
      procedure Add_File (Kind : Content_Type);
      procedure Add_File (Kind : Content_Type) is
         Info : constant File_Info_P
           := new File_Info'(Name => new String'(Named),
                             Kind => Kind,
                             Next => null);
      begin
         if First_File = null then
            First_File := Info;
            Last_File := Info;
         else
            Last_File.Next := Info;
            Last_File := Info;
         end if;
      end Add_File;
   begin
      if Match (CSS_File, Named) >= Named'First then
         Add_File (CSS);
      elsif Match (GIF_File, Named) >= Named'First then
         Add_File (GIF);
      elsif Match (HTML_File, Named) >= Named'First then
         Add_File (HTML);
      elsif Match (ICO_File, Named) >= Named'First then
         Add_File (ICO);
      elsif Match (Java_File, Named) >= Named'First then
         Add_File (Octet_Stream);
      elsif Match (JPEG_File, Named) >= Named'First then
         Add_File (JPEG);
      elsif Match (JavaScript_File, Named) >= Named'First then
         Add_File (JavaScript);
      elsif Match (PNG_File, Named) >= Named'First then
         Add_File (PNG);
      elsif Match (XML_File, Named) >= Named'First then
         Add_File (XML);
      elsif Match (XSL_File, Named) >= Named'First then
         Add_File (XSL);
      end if;
   end Save_File;

   procedure Output (Base_Dir : String) is
      Base_Dir_Len : constant Positive := Base_Dir'Length;
      Id : Positive;
      F : File_Info_P;
      function Image (P : Positive) return String;
      function Image (F : String) return String;
      function Image (P : Positive) return String is
         Res : constant String := Positive'Image (P);
      begin
         return Res (Res'First + 1 .. Res'Last);
      end Image;
      function Image (F : String) return String is
         Start : constant Positive := F'First + Base_Dir_Len - 1;
         Result : String := F (Start .. F'Last);
      begin
         for C in Result'Range loop
            if Result (C) = '\' then
               Result (C) := '/';
            end if;
         end loop;
         return Result;
      end Image;
   begin
      F := First_File;
      Id := 1;
      while F /= null loop
         Put_Line ("   Url_"
                     & Image (Id)
                     & " : aliased constant String := """
                     & Image (F.Name.all)
                     & """;");
         Put ("   Doc_"
                & Image (Id)
                & " : aliased constant Stream_Element_Array");
         Output_Contents (F.Name.all);
         F := F.Next;
         Id := Id + 1;
      end loop;
      Put_Line ("   Documents : aliased constant Url_Info_Array :=");
      Put_Line ("     (");
      F := First_File;
      Id := 1;
      while F /= null loop
         Put ("      "
                & Image (Id)
                & " => (Url => Url_"
                & Image (Id)
                & "'Access, Doc => Doc_"
                & Image (Id)
                & "'Access, Form => "
                & F.Kind'Img
                & ")");
         if F.Next /= null then
            Put (",");
         end if;
         New_Line;
         F := F.Next;
         Id := Id + 1;
      end loop;
      Put_Line ("     );");
   end Output;

   procedure Output_Contents (Of_File : String) is
      File : Stream_IO.File_Type;
      Line : Stream_Element_Array (1 .. 12);
      Last : Stream_Element_Offset;
   begin
      Open (File, Mode => In_File, Name => Of_File);
      Read (File, Line, Last);
      if Last >= 1 then
         --  we have something to output
         Put_Line (" :=");
         Put_Line ("     (");
         Put ("     ");
         for C in 1 .. Last - 1 loop
            Put (Line (C)'Img);
            Put (",");
         end loop;
         Put (Line (Last)'Img);
         if not End_Of_File (File) then
            Put (",");
         end if;
         New_Line;
         loop
            exit when End_Of_File (File);
            Read (File, Line, Last);
            if Last >= 1 then
               --  we have something to output
               Put ("     ");
               for C in 1 .. Last - 1 loop
                  Put (Line (C)'Img);
                  Put (",");
               end loop;
               Put (Line (Last)'Img);
               if not End_Of_File (File) then
                  Put (",");
               end if;
               New_Line;
            end if;
         end loop;
         Put_Line ("     );");
      else
         --  we need an empty array
         Put_Line (" := (0 => 0);");
      end if;
      Close (File);
   end Output_Contents;

begin

   case Ada.Command_Line.Argument_Count is
      when 0 =>
         null;
      when 1 =>
         begin
            Change_Dir (Ada.Command_Line.Argument (1));
         exception
            when Directory_Error =>
               Put_Line (Standard_Error,
                         "unable to open " & Ada.Command_Line.Argument (1));
               Ada.Command_Line.Set_Exit_Status
                 (Ada.Command_Line.Failure);
               return;
         end;
      when others =>
         Put_Line (Standard_Error,
                   "may only specify one directory");
         Ada.Command_Line.Set_Exit_Status
           (Ada.Command_Line.Failure);
         return;
   end case;

   Put_Line ("pragma Style_Checks (Off);");
   Put_Line ("--  Generated by ews-make_htdocs");
   Put_Line ("--  Source: " & Get_Current_Dir);
   Put_Line ("with Ada.Streams; use Ada.Streams;");
   Put_Line ("with EWS.Types; use EWS.Types;");
   Put_Line ("package body EWS.Htdocs is");

   Scan_Directory (Get_Current_Dir);
   Output (Get_Current_Dir);

   Put_Line ("   function Static_Urls return Url_Info_Array_P is");
   Put_Line ("   begin");
   Put_Line ("      return Documents'Access;");
   Put_Line ("   end Static_Urls;");
   Put_Line ("end EWS.Htdocs;");

end EWS.Make_Htdocs;
