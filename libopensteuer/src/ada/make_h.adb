with Ada.Text_IO;
with Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;
with GNAT.Regpat; use GNAT.Regpat;

-----------------------------------------------------
-- Generate libopensteuer.h from libopensteuer.ads --
-----------------------------------------------------

procedure Make_h is
   
   --------------------
   -- File-functions --
   --------------------
   
   package ATIO renames Ada.Text_IO;
   package AS renames Ada.Strings;
   package ASF renames Ada.Strings.Fixed;
   package ASMC renames Ada.Strings.Maps.Constants;
   
   ads_File, h_File : ATIO.File_Type;
   
   function Open_ads_File return Boolean is
   begin
      ATIO.Open (ads_File, ATIO.In_File, "libopensteuer.ads");
      return True;
   exception
      when ATIO.Name_Error | ATIO.Use_Error => return False;
   end Open_ads_File;
   
   function Create_h_File return Boolean is
   begin
      ATIO.Create (h_File, ATIO.Out_File, "libopensteuer.h");
      return True;
   exception
      when ATIO.Name_Error | ATIO.Use_Error => return False;
   end Create_h_File;
   
   procedure Close_ads_File is
   begin
      ATIO.Close (ads_File);
   end Close_ads_File;
   
   procedure Close_h_File is
   begin
      ATIO.Close (h_File);
   end Close_h_File;
   
   function End_Of_ads_File return Boolean is
   begin
      return ATIO.End_Of_File (ads_File);
   end End_Of_ads_File;
   
   -------------
   -- Convert --
   -------------
   
   -- Comments:
   
   procedure Convert_Comment (s : String) is
      Regexp_Comment : String := "-- +(.+) +--";
      Match_Comment : Pattern_Matcher := Compile (Regexp_Comment);
      Array_Comment : Match_Array(0.. Paren_Count (Match_Comment));
   begin
      Match (Match_Comment, s, Array_Comment);
      if Array_Comment(1).First > 0 then
         ATIO.Put_Line (h_File, " ");
         ATIO.Put_Line (h_File, "/* " & s(Array_Comment(1).First .. Array_Comment(1).Last) & " */");
         ATIO.Put_Line (h_File, " ");
      end if;
   end Convert_Comment;
   
   -- Types:
   
   subtype Types_Index_type is Natural range 1 .. 2;
   type Types_type is array(Types_Index_type'Range, 1 .. 2) of String(1 .. 40);
   
   Types_Index : Types_Index_type := Types_Index_type'First;
   Types : Types_type;
   
   procedure Get_Types (s : String) is
      Regexp_Types : String := "type (\w) is new interfaces.c.([a-z]+)";
      Match_Types : Pattern_Matcher := Compile (Regexp_Types);
      Array_Types : Match_Array(0.. Paren_Count (Match_Types));
   begin
      Match (Match_Types, s, Array_Types);
      if Array_Types(1).First > 0 then
         ASF.Move (s(Array_Types(1).First .. Array_Types(1).Last), Types(Types_Index, 1));
         ASF.Move (s(Array_Types(2).First .. Array_Types(2).Last), Types(Types_Index, 2));
         Types_Index := Types_Index_type'Succ (Types_Index);
      end if;
   end Get_Types;
   
   procedure Convert_Types (s : String) is
   begin
      for i in Types_Index_type'First .. Types_Index loop
         if s = ASF.Trim (Types(i, 1), AS.Both) then
            ATIO.Put (h_File, ASF.Trim (Types(i, 2), AS.Both));
         end if;
      end loop;
   end Convert_Types;
   
   -- Error_Codes:
   
   procedure Convert_Error (s : String) is
      Regexp_Error : String := "(.*_code) +: +constant +(.*_type_c) +:= +(-?[0-9]+);";
      Match_Error : Pattern_Matcher := Compile (Regexp_Error);
      Array_Error : Match_Array(0.. Paren_Count (Match_Error));
   begin
      Match (Match_Error, s, Array_Error);
      if Array_Error(0).First > 0 then
         Convert_Types (s(Array_Error(2).First .. Array_Error(2).Last));
         ATIO.Put (h_File, " " & s(Array_Error(1).First .. Array_Error(1).Last));
         ATIO.Put (h_File, " = " & s(Array_Error(3).First .. Array_Error(3).Last));
         ATIO.Put_Line (h_File, ";");
      end if;
   end Convert_Error;
   
   -- Functions:
   
   procedure Convert_Function (s : String) is
      Regexp_Function : String := "function +([a-z0-9_]+)_c(.*) +return +(.+);";
      Match_Function : Pattern_Matcher := Compile (Regexp_Function);
      Array_Function : Match_Array(0 .. Paren_Count (Match_Function));
      
      Regexp_Braces : String := "\(([a-z0-9_]+) *: +in +(.*)\)";
      Match_Braces : Pattern_Matcher := Compile (Regexp_Braces);
      Array_Braces : Match_Array(0 .. Paren_Count (Match_Braces));
   begin
      Match (Match_Function, s, Array_Function);
      -- First the return-type:
      if Array_Function(3).First > 0 then
         Convert_Types (s(Array_Function(3).First .. Array_Function(3).Last));
      end if;
      -- Then the function-name:
      if Array_Function(1).First > 0 then
         ATIO.Put (h_File, " " & s(Array_Function(1).First .. Array_Function(1).Last));
      end if;
      -- Now convert the braces, if there are any:
      if Array_Function(2).First > 0 then
         ATIO.Put (h_File, " (");
         Match (Match_Braces, s(Array_Function(2).First .. Array_Function(2).Last), Array_Braces);
         if Array_Braces(2).First > 0 then
            Convert_Types (ASF.Trim (s(Array_Braces(2).First .. Array_Braces(2).Last), AS.Both));
            --ATIO.Put (h_File, s(Array_Braces(2).First .. Array_Braces(2).Last));
            ATIO.Put (h_File, " " & s(Array_Braces(1).First .. Array_Braces(1).Last));
         else
            ATIO.Put (h_File, "void");
         end if;
         ATIO.Put (h_File, ");");
      end if;
      ATIO.New_Line (h_File);
   end Convert_Function;
   
   s : String(1 .. 1024);
   len : Integer;
   begin
      if Open_ads_File then
         if Create_h_File then
            ATIO.Put_Line (h_File, "/* This file is generated from libopensteuer.ads by make_h */");
            while not End_Of_ads_File loop
               ATIO.Get_Line (ads_File, s, len);
               declare
                  t : String := ASF.Trim (s(1 .. len), AS.Both);
               begin
                  -- Convert comment:
                  if ASF.Index (t, "--") > 0 then
                     -- we define no types:
                     if ASF.Index (t, "ype") = 0 then
                        Convert_Comment (t);
                     end if;
                  end if;
                  ASF.Translate (t, ASMC.Lower_Case_Map);
                  -- Get the C-type of our types:
                  if ASF.Index (t, "type") > 0 then
                     if ASF.Index (t, "interfaces.c.") > 0 then
                        Get_Types (t);
                     end if;
                  end if;
                  -- Convert functions:
                  if ASF.Index (t, "_c") > 0 then
                     if ASF.Index (t, "function") > 0 then
                        Convert_Function (t);
                     elsif ASF.Index (t, "error") > 0 then
                        Convert_Error (t);
                     end if;
                  end if;
               end;
            end loop;
            Close_h_File;
         end if;
         Close_ads_File;
      end if;
end Make_h;
