package LibOpenSteuer.EstTab is
   
   procedure Set_ZVE (ZVE : in Cent_Type);
   function Get_Grundtab return Cent_Type;
   function Get_Splittab return Cent_Type;
   
   procedure Calc_Est;
   
private

   My_ZVE : Cent_Type := 0;

end LibOpenSteuer.EstTab;
