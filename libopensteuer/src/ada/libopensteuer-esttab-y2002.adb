with LibOpenSteuer.PAP; use LibOpenSteuer.PAP;

package body LibOpenSteuer.EstTab.Y2002 is
   
   function Get_Grundtab return Cent_Type is
   begin
      return Grundtab;
   end Get_Grundtab;
   
   function Get_Splittab return Cent_Type is
   begin
      return Splittab;
   end Get_Splittab;
   
   procedure Calc_Est is
   begin
      Reset_PAP_Intern;
      Set_KZTAB (1);
      PAP.Set_ZVE (My_ZVE);
      PAP.EstTab;
      Grundtab := Get_ST;

      Reset_PAP_Intern;
      Set_KZTAB (2);
      PAP.Set_ZVE (My_ZVE);
      PAP.EstTab;
      Splittab := Get_ST;
   end Calc_Est;
   
end LibOpenSteuer.EstTab.Y2002;
