with Ada.Text_IO;
with Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;
with GNAT.Regpat; use GNAT.Regpat;

-----------------------------------------------------
-- Generate libopensteuer.h from libopensteuer.ads --
-----------------------------------------------------

procedure Make_h is
   
   -- File-functions:
   
   package ATI renames Ada.Text_IO;
   package AS renames Ada.Strings;
   package ASF renames Ada.Strings.Fixed;
   package ASMC renames Ada.Strings.Maps.Constants;
   
   ads_File, h_File : ATI.File_Type;
   
   function Open_ads_File return Boolean is
   begin
      ATI.Open (ads_File, ATI.In_File, "libopensteuer.ads");
      return True;
   exception
      when ATI.Name_Error | ATI.Use_Error => return False;
   end Open_ads_File;
   
   function Create_h_File return Boolean is
   begin
      ATI.Create (h_File, ATI.Out_File, "libopensteuer.h");
      return True;
   exception
      when ATI.Name_Error | ATI.Use_Error => return False;
   end Create_h_File;
   
   procedure Close_ads_File is
   begin
      ATI.Close (ads_File);
   end Close_ads_File;
   
   procedure Close_h_File is
   begin
      ATI.Close (h_File);
   end Close_h_File;
   
   function End_Of_ads_File return Boolean is
   begin
      return ATI.End_Of_File (ads_File);
   end End_Of_ads_File;
   
   -- Convert:
   
   procedure Get_Type (s : String) is
   begin
      null;
   end Get_Type;
   
   -- Convert Comment:
   
   Regexp_Comment : String := "-- +(.+) +--";
   Match_Comment : Pattern_Matcher := Compile (Regexp_Comment);
   Array_Comment : Match_Array(0.. Paren_Count (Match_Comment));
   
   procedure Convert_Comment (s : String) is
   begin
      Match (Match_Comment, s, Array_Comment);
      if Array_Comment(1).First > 0 then
         ATI.Put_Line (h_File, " ");
         ATI.Put_Line (h_File, "/* " & s(Array_Comment(1).First .. Array_Comment(1).Last) & " */");
         ATI.Put_Line (h_File, " ");
      end if;
   end Convert_Comment;
   
   -- Convert Type:
   
   procedure Convert_Type (s : String) is
   begin
      if s = "general_type_c" then
         ATI.Put (h_File, "int");
      elsif s = "cent_type_c" then
         ATI.Put (h_File, "long");
      end if;
   end Convert_Type;
   
   -- Convert Error_Code:
   
   Regexp_Error : String := "(.*_code) +: +constant +(.*_type_c) +:= +(-?[0-9]+);";
   Match_Error : Pattern_Matcher := Compile (Regexp_Error);
   Array_Error : Match_Array(0.. Paren_Count (Match_Error));
   
   procedure Convert_Error (s : String) is
   begin
      Match (Match_Error, s, Array_Error);
      if Array_Error(0).First > 0 then
         Convert_Type (s(Array_Error(2).First .. Array_Error(2).Last));
         ATI.Put (h_File, " " & s(Array_Error(1).First .. Array_Error(1).Last));
         ATI.Put (h_File, " = " & s(Array_Error(3).First .. Array_Error(3).Last));
         ATI.Put_Line (h_File, ";");
      end if;
   end Convert_Error;
   
   -- Convert Function:
   
   Regexp_Function : String := "function +([a-z0-9_]+)_c(.*) +return +(.+);";
   Match_Function : Pattern_Matcher := Compile (Regexp_Function);
   Array_Function : Match_Array(0 .. Paren_Count (Match_Function));
   
   Regexp_Braces : String := "\(([a-z0-9_]+) *: +in +(.*)\)";
   Match_Braces : Pattern_Matcher := Compile (Regexp_Braces);
   Array_Braces : Match_Array(0 .. Paren_Count (Match_Braces));
   
   procedure Convert_Function (s : String) is
   begin
      Match (Match_Function, s, Array_Function);
      -- First the return-type:
      if Array_Function(3).First > 0 then
         Convert_Type (s(Array_Function(3).First .. Array_Function(3).Last));
      end if;
      -- Then the function-name:
      if Array_Function(1).First > 0 then
         ATI.Put (h_File, " " & s(Array_Function(1).First .. Array_Function(1).Last));
      end if;
      -- Now convert the braces, if there are any:
      if Array_Function(2).First > 0 then
         ATI.Put (h_File, " (");
         Match (Match_Braces, s(Array_Function(2).First .. Array_Function(2).Last), Array_Braces);
         if Array_Braces(2).First > 0 then
            Convert_Type (ASF.Trim (s(Array_Braces(2).First .. Array_Braces(2).Last), AS.Both));
            --ATI.Put (h_File, s(Array_Braces(2).First .. Array_Braces(2).Last));
            ATI.Put (h_File, " " & s(Array_Braces(1).First .. Array_Braces(1).Last));
         else
            ATI.Put (h_File, "void");
         end if;
         ATI.Put (h_File, ");");
      end if;
      ATI.New_Line (h_File);
   end Convert_Function;
   
   s : String(1 .. 1024);
   len : Integer;
   
   begin
      if Open_ads_File then
         if Create_h_File then
            while not End_Of_ads_File loop
               ATI.Get_Line (ads_File, s, len);
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
                        Get_Type (t);
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
