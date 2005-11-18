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

--  $RCSfile$
--  $Revision$
--  $Date$
--  $Author$

with Ada.Text_IO; use Ada.Text_IO;

package body EWS_Pages_Support is

   procedure Colophon;
   procedure Front_Matter;


   function "+"
     (S : String)
     return Ada.Strings.Unbounded.Unbounded_String
     renames Ada.Strings.Unbounded.To_Unbounded_String;


   function "+"
     (S : Ada.Strings.Unbounded.Unbounded_String)
     return String
     renames Ada.Strings.Unbounded.To_String;


   procedure Colophon is
   begin
      Put_Line ("return Result;");
      Put_Line ("end Page;");
   end Colophon;


   procedure Front_Matter is
   begin
      Put_Line ("with Ada.Characters.Latin_1;");
      Put_Line ("with EWS.Types;");
      Put_Line ("pragma Style_Checks (Off);");
      Put_Line ("function Page");
      Put_Line ("(From_Request : EWS.HTTP.Request_P)");
      Put_Line ("return EWS.Dynamic.Dynamic_Response'Class is");
      Put_Line ("use Ada.Characters.Latin_1;");
      Put_Line ("Result : EWS.Dynamic.Dynamic_Response (From_Request);");
      Put_Line ("use EWS.Dynamic;");
      Put_Line ("use EWS.Types;");
      Put_Line ("begin");
      Put_Line ("Set_Content_Type (Result, To => HTML);");
   end Front_Matter;


   procedure Output (This : access Context) is
   begin
      Put_Line ("with " & (+This.Text) & ";");
   end Output;


   procedure Output (This : access Variable) is
      Unimplemented : exception;
   begin
      raise Unimplemented;
   end Output;


   procedure Output (This : access Code) is
   begin
      Put_Line ("Append (Result, """ & (+This.Text) & """ & CR & LF);");
   end Output;


   procedure Output (This : access Literal) is
      Escaped : Ada.Strings.Unbounded.Unbounded_String;
      use Ada.Strings.Unbounded;
   begin
      for I in 1 .. Length (This.Text) loop
         if Element (This.Text, I) = '"' then
            Append (Escaped, """""");
         else
            Append (Escaped, Element (This.Text, I));
         end if;
      end loop;
      Put_Line ("Append (Result, """ & (+Escaped) & """ & CR & LF);");
   end Output;


   procedure Add_Context (S : String; To : in out Compiled_Page) is
   begin
      Collections.Append
        (To.Context, new Context'(Text => +S));
   end Add_Context;


   procedure Add_Text (S : String; To : in out Compiled_Page) is

      use type GNAT.Regpat.Regexp_Flags;

      Line_Regexp : constant String := "^(.*)$";

      Line_Matcher : constant GNAT.Regpat.Pattern_Matcher :=
        GNAT.Regpat.Compile (Line_Regexp,
                             Flags => GNAT.Regpat.Multiple_Lines);

      Line_Max_Parens : constant GNAT.Regpat.Match_Count :=
        GNAT.Regpat.Paren_Count (Line_Matcher);

      Matches : GNAT.Regpat.Match_Array (0 .. Line_Max_Parens);

      Start : Positive := S'First;

      use type GNAT.Regpat.Match_Location;

   begin

      loop

         GNAT.Regpat.Match (Line_Matcher, S, Matches, Data_First => Start);
         exit when Matches (0) = GNAT.Regpat.No_Match;

         if Matches (0).Last > Matches (0).First then
            Collections.Append
              (To.Text, new Literal'(Text => +To_String (S, Matches, 0)));
         end if;

         Start := Matches (0).Last + 2; -- skip the \n
         exit when Start > S'Last;

      end loop;

   end Add_Text;


   procedure Add_Code (S : String; To : in out Compiled_Page) is
   begin
      Collections.Append
        (To.Text, new Code'(Text => +S));
   end Add_Code;


   procedure Output (This : Compiled_Page) is
      Context : Abstract_Containers.Iterator'Class
        := Collections.New_Iterator (This.Context);
      Text : Abstract_Containers.Iterator'Class
        := Collections.New_Iterator (This.Text);
      use Abstract_Containers;
   begin
      while not Is_Done (Context) loop
         Output (Current_Item (Context));
         Next (Context);
      end loop;
      Front_Matter;
      while not Is_Done (Text) loop
         Output (Current_Item (Text));
         Next (Text);
      end loop;
      Colophon;
   end Output;


   function To_String
     (In_String : String;
      From : GNAT.Regpat.Match_Array;
      At_Location : Natural) return String is
   begin
      return In_String (From (At_Location).First .. From (At_Location).Last);
   exception
      when others => return "";
   end To_String;


end EWS_Pages_Support;
