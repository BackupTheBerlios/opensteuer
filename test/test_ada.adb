with Text_IO; use Text_IO;
with libopensteuer; use libopensteuer;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

procedure test_ada is
   First_Year, Last_Year : Ada_Integer;
begin
   New_Line;
   Put_Line ("Hier ist das Ada-Programm...");
   New_Line;
   Put_Line ("Lohnsteuerberechnung (alle Beträge in Euro):");
   First_Year := Get_First_Year;
   Last_Year := Get_Last_Year;
   for Jahr in First_Year .. Last_Year loop
      New_Line;
      Put_Line ("Jahr:" & Ada_Integer'Image (Jahr));
      Put_Line ("     RE4       I      II     III      IV       V      VI");
      Put_Line ("--------------------------------------------------------");
      for r in 2..24 loop
         Reset_All;
         Set_Year (Ada_Integer (Jahr));
         Set_LZZ (1);
         Set_RE4 (Ada_Cent (r) * 2_500_00);
         Put (Tail (Ada_Cent'Image (Ada_Cent (r) * 2500), 8));
         for i in 1..6 loop
            Set_STKL (Ada_Integer (i));
            Calc_Lst;
            Put (Tail (Ada_Cent'Image (Get_LSTLZZ / 100), 8));
      end loop;
      New_Line;
      end loop;
   end loop;
   New_Line;
   Put_Line ("Einkommensteuerberechnung (alle Beträge in Euro):");
   for Jahr in First_Year .. Last_Year loop
      New_Line;
      Put_Line ("Jahr:" & Ada_Integer'Image (Jahr));
      Put_Line ("     ZVE        Grundtabelle     Splittingtabelle");
      Put_Line ("-------------------------------------------------");
      for r in 1 .. 10 loop
         Reset_All;
         Set_Year (Ada_Integer (Jahr));
         Set_LZZ (1);
         Set_ZVE (Ada_Cent (r) * 10_000_00);
         Put (Tail (Ada_Cent'Image (Ada_Cent (r) * 10_000), 8));
         Calc_Est;
         Put (Tail (Ada_Cent'Image (Get_Grundtab / 100), 20));
         Put (Tail (Ada_Cent'Image (Get_Splittab / 100), 20));
         New_Line;
      end loop;
   end loop;
   New_Line;
end test_ada;
