with Ada.Exceptions; use Ada.Exceptions;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;
with LibOpenSteuer; use LibOpenSteuer;

------------------------------------------------------------
-- Simple program to demonstrate the use of libopensteuer --
-- Author: Martin Klaiber.                                --
------------------------------------------------------------

procedure Test_Ada is
begin
   New_Line; Put_Line ("This is the Ada-program...");
   New_Line; Put_Line ("Lohnsteuer (all amounts in Euro):");
   -- Reset_All;
   -- Force an exception in Set_Year (Year):
   for Year in Get_First_Year - 1 .. Get_Last_Year loop
      declare
      begin
         New_Line; Put_Line ("Year:" & General_Type'Image (Year));
         Set_Year (Year);
         Put_Line ("     RE4       I      II     III      IV       V      VI");
         Put_Line ("--------------------------------------------------------");
         for r in 2..24 loop
            Reset_All;
            Set_Year (Year);
            Set_LZZ (1);
            -- The library expects amounts in Euro-Cent:
            Set_RE4 (Cent_Type (r) * 2_500_00);
            Put (Tail (Cent_Type'Image (Cent_Type (r) * 2500), 8));
            for i in 1..6 loop
               Set_STKL (General_Type (i));
               -- Start the calculation after all settings are done:
               Calc_Lst;
               -- The library also returns all amounts in Euro-Cent:
               Put (Tail (Cent_Type'Image (Get_LSTLZZ / 100), 8));
            end loop;
            New_Line;
         end loop;
      exception
         when E : others =>
            Put_Line ("Error: " & Exception_Name (E));
            Put_Line ("Location: " & Exception_Message (E));
      end;
   end loop;
   New_Line;
   Put_Line ("Einkommensteuer (all amounts in Euro):");
   for Year in Get_First_Year .. Get_Last_Year loop
      New_Line;
      Put_Line ("Year:" & General_Type'Image (Year));
      Put_Line ("     ZVE        Grundtabelle     Splittingtabelle");
      Put_Line ("-------------------------------------------------");
      for r in 1 .. 10 loop
         Reset_All;
         Set_Year (Year);
         Set_LZZ (1);
         Set_ZVE (Cent_Type (r) * 10_000_00);
         Put (Tail (Cent_Type'Image (Cent_Type (r) * 10_000), 8));
         Calc_Est;
         Put (Tail (Cent_Type'Image (Get_Grundtab / 100), 20));
         Put (Tail (Cent_Type'Image (Get_Splittab / 100), 20));
         New_Line;
      end loop;
   end loop;
   New_Line;
end test_ada;
