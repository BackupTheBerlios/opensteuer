with Text_IO; use Text_IO;
with libopensteuer; use libopensteuer;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

procedure test_ada is
begin
   New_Line;
   Put_Line ("Hier ist das Ada-Programm...");
   New_Line;
   Put_Line ("Lohnsteuerberechnung (alle Beträge in Euro):");
   for Jahr in Get_First_Year .. Get_Last_Year loop
      New_Line;
      Put_Line ("Jahr:" & General_Type'Image (Jahr));
      Put_Line ("     RE4       I      II     III      IV       V      VI");
      Put_Line ("--------------------------------------------------------");
      for r in 2..24 loop
         Reset_All;
         Set_Year (Jahr);
         Set_LZZ (1);
         Set_RE4 (Cent_Type (r) * 2_500_00);
         Put (Tail (Cent_Type'Image (Cent_Type (r) * 2500), 8));
         for i in 1..6 loop
            Set_STKL (General_Type (i));
            Calc_Lst;
            Put (Tail (Cent_Type'Image (Get_LSTLZZ / 100), 8));
         end loop;
         New_Line;
      end loop;
   end loop;
   New_Line;
   Put_Line ("Einkommensteuerberechnung (alle Beträge in Euro):");
   for Jahr in Get_First_Year .. Get_Last_Year loop
      New_Line;
      Put_Line ("Jahr:" & General_Type'Image (Jahr));
      Put_Line ("     ZVE        Grundtabelle     Splittingtabelle");
      Put_Line ("-------------------------------------------------");
      for r in 1 .. 10 loop
         Reset_All;
         Set_Year (Jahr);
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
