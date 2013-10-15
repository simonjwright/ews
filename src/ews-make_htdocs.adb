--  This package is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or
--  (at your option) any later version.  It is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE.
--
--  You should have received a copy of the GNU General Public License
--  along with this program; see the file COPYING3. If not, see
--  <http://www.gnu.org/licenses/>.
--
--  Copyright Simon Wright <simon@pushface.org>

with Ada.Command_Line;
with Ada.Streams.Stream_IO;
with Ada.Text_IO; use Ada.Text_IO;
with EWS.Types;
with GNAT.Directory_Operations;
with GNAT.OS_Lib;
with GNAT.Regpat;

procedure EWS.Make_Htdocs is

   procedure Scan_Directory (Named : GNAT.Directory_Operations.Dir_Name_Str);
   procedure Save_File (Named : String);
   procedure Output (Base_Dir : String);
   procedure Output_Contents (Of_File : String);

   --  These two procedures are in a package because they need to keep
   --  the actual open file that's associated with standard output
   --  available so that it can be closed; and no one else should be
   --  able to see the file.
   package Output_Management is
      procedure Set_Standard_Output (To_File_Named : String);
      procedure Reset_Standard_Output;
   end Output_Management;

   procedure Scan_Directory (Named : GNAT.Directory_Operations.Dir_Name_Str) is
      Wd : GNAT.Directory_Operations.Dir_Type;
   begin
      GNAT.Directory_Operations.Open (Dir => Wd,
                                      Dir_Name => Named);
      declare
         Str : String (1 .. 1024);
         Last : Natural;
      begin
         loop
            GNAT.Directory_Operations.Read (Dir => Wd,
                                            Str => Str,
                                            Last => Last);
            exit when Last = 0;
            if Str (1 .. Last) /= "." and Str (1 .. Last) /= ".." then
               if GNAT.OS_Lib.Is_Directory (Named & Str (1 .. Last)) then
                  Scan_Directory
                    (Named & Str (1 .. Last)
                       & GNAT.OS_Lib.Directory_Separator);
               else
                  Save_File (Named & Str (1 .. Last));
               end if;
            end if;
         end loop;
      end;
      GNAT.Directory_Operations.Close (Dir => Wd);
   end Scan_Directory;

   CSS_File : constant GNAT.Regpat.Pattern_Matcher
     := GNAT.Regpat.Compile ("\.css$");
   GIF_File : constant GNAT.Regpat.Pattern_Matcher
     := GNAT.Regpat.Compile ("\.gif$");
   HTML_File : constant GNAT.Regpat.Pattern_Matcher
     := GNAT.Regpat.Compile ("\.(html|htm)$");
   ICO_File : constant GNAT.Regpat.Pattern_Matcher
     := GNAT.Regpat.Compile ("\.ico$");
   JPEG_File : constant GNAT.Regpat.Pattern_Matcher
     := GNAT.Regpat.Compile ("\.(jpeg|jpg)$");
   JavaScript_File : constant GNAT.Regpat.Pattern_Matcher
     := GNAT.Regpat.Compile ("\.js$");
   Java_File : constant GNAT.Regpat.Pattern_Matcher
     := GNAT.Regpat.Compile ("\.(class|jar)$");
   PDF_File : constant GNAT.Regpat.Pattern_Matcher
     := GNAT.Regpat.Compile ("\.pdf$");
   PNG_File : constant GNAT.Regpat.Pattern_Matcher
     := GNAT.Regpat.Compile ("\.png$");
   XML_File : constant GNAT.Regpat.Pattern_Matcher
     := GNAT.Regpat.Compile ("\.xml$");
   XSL_File : constant GNAT.Regpat.Pattern_Matcher
     := GNAT.Regpat.Compile ("\.xsl$");

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
      if GNAT.Regpat.Match (CSS_File, Named) >= Named'First then
         Add_File (EWS.Types.CSS);
      elsif GNAT.Regpat.Match (GIF_File, Named) >= Named'First then
         Add_File (EWS.Types.GIF);
      elsif GNAT.Regpat.Match (HTML_File, Named) >= Named'First then
         Add_File (EWS.Types.HTML);
      elsif GNAT.Regpat.Match (ICO_File, Named) >= Named'First then
         Add_File (EWS.Types.ICO);
      elsif GNAT.Regpat.Match (Java_File, Named) >= Named'First then
         Add_File (EWS.Types.Octet_Stream);
      elsif GNAT.Regpat.Match (JPEG_File, Named) >= Named'First then
         Add_File (EWS.Types.JPEG);
      elsif GNAT.Regpat.Match (JavaScript_File, Named) >= Named'First then
         Add_File (EWS.Types.JavaScript);
      elsif GNAT.Regpat.Match (PDF_File, Named) >= Named'First then
         Add_File (EWS.Types.PDF);
      elsif GNAT.Regpat.Match (PNG_File, Named) >= Named'First then
         Add_File (EWS.Types.PNG);
      elsif GNAT.Regpat.Match (XML_File, Named) >= Named'First then
         Add_File (EWS.Types.XML);
      elsif GNAT.Regpat.Match (XSL_File, Named) >= Named'First then
         Add_File (EWS.Types.XSL);
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
      procedure Output_Line;
      File : Ada.Streams.Stream_IO.File_Type;
      Line : Ada.Streams.Stream_Element_Array (1 .. 12);
      Last : Ada.Streams.Stream_Element_Offset;
      use type Ada.Streams.Stream_Element_Offset;
      procedure Output_Line is
         --  Output the bytes of Line (1 .. Last), comma-separated,
         --  and with a trailing comma if there's more to come.
      begin
         Put ("     ");
         for C in 1 .. Last - 1 loop
            Put (Line (C)'Img);
            Put (",");
         end loop;
         Put (Line (Last)'Img);
         if not Ada.Streams.Stream_IO.End_Of_File (File) then
            Put (",");
         end if;
         New_Line;
      end Output_Line;
   begin
      Ada.Streams.Stream_IO.Open (File,
                                  Mode => Ada.Streams.Stream_IO.In_File,
                                  Name => Of_File);
      Ada.Streams.Stream_IO.Read (File, Line, Last);
      if Last >= 1 then
         --  we have something to output
         Put_Line (" :=");
         Put_Line ("     (");
         Output_Line;
         loop
            exit when Ada.Streams.Stream_IO.End_Of_File (File);
            Ada.Streams.Stream_IO.Read (File, Line, Last);
            if Last >= 1 then
               --  we have something to output
               Output_Line;
            end if;
         end loop;
         Put_Line ("     );");
      else
         --  we need an empty array
         Put_Line (" := (0 => 0);");
      end if;
      Ada.Streams.Stream_IO.Close (File);
   end Output_Contents;

   package body Output_Management is

      File : File_Type;

      procedure Set_Standard_Output (To_File_Named : String)
      is
      begin
         Open (File => File,
               Mode => Out_File,
               Name => To_File_Named);
         Set_Output (File);
      exception
         when Name_Error =>
            Create (File => File,
                    Name => To_File_Named);
            Set_Output (File);
      end Set_Standard_Output;

      procedure Reset_Standard_Output is
      begin
         Close (File);
         Set_Output (Standard_Output);
      end Reset_Standard_Output;

   end Output_Management;

   Starting_Dir : constant GNAT.Directory_Operations.Dir_Name_Str
     := GNAT.Directory_Operations.Get_Current_Dir;

begin

   case Ada.Command_Line.Argument_Count is
      when 0 =>
         null;
      when 1 =>
         begin
            GNAT.Directory_Operations.Change_Dir
              (Ada.Command_Line.Argument (1));
         exception
            when GNAT.Directory_Operations.Directory_Error =>
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

   Output_Management.Set_Standard_Output (Starting_Dir & "ews_htdocs.ads");
   Put_Line ("--  Generated by ews-make_htdocs");
   Put_Line ("--  Source: " & GNAT.Directory_Operations.Get_Current_Dir);
   Put_Line ("package EWS_Htdocs with Elaborate_Body is");
   Put_Line ("end EWS_Htdocs;");
   Output_Management.Reset_Standard_Output;

   Output_Management.Set_Standard_Output (Starting_Dir & "ews_htdocs.adb");
   Put_Line ("pragma Style_Checks (Off);");
   Put_Line ("--  Generated by ews-make_htdocs");
   Put_Line ("--  Source: " & GNAT.Directory_Operations.Get_Current_Dir);
   Put_Line ("with Ada.Streams; use Ada.Streams;");
   Put_Line ("with EWS.Static; use EWS.Static;");
   Put_Line ("with EWS.Types; use EWS.Types;");
   Put_Line ("package body EWS_Htdocs is");

   Scan_Directory (GNAT.Directory_Operations.Get_Current_Dir);
   Output (GNAT.Directory_Operations.Get_Current_Dir);

   Put_Line ("begin");
   Put_Line ("   Register (Documents'Access);");
   Put_Line ("end EWS_Htdocs;");
   Output_Management.Reset_Standard_Output;

end EWS.Make_Htdocs;
