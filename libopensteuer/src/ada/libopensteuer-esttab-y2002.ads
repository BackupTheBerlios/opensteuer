package LibOpenSteuer.EstTab.Y2002 is
   
   function Get_Grundtab return Cent_Type;
   function Get_Splittab return Cent_Type;
   procedure Calc_Est;
   
private

   Grundtab, Splittab : Cent_Type := 0;

end LibOpenSteuer.EstTab.Y2002;
