with LibOpenSteuer.EstTab.Y2002;
with LibOpenSteuer.EstTab.Y2004;

package body LibOpenSteuer.EstTab is
   
   procedure Set_ZVE (ZVE : in Cent_Type) is
   begin
      My_ZVE := ZVE;
   end Set_ZVE;
   
   function Get_Grundtab return Cent_Type is
   begin
      case Tax_Year is
         when 2002 | 2003 => return Y2002.Get_Grundtab;
         when 2004 => return Y2004.Get_Grundtab;
      end case;
   end Get_Grundtab;
   
   function Get_Splittab return Cent_Type is
   begin
      case Tax_Year is
         when 2002 | 2003 => return Y2002.Get_Splittab;
         when 2004 => return Y2004.Get_Splittab;
      end case;
   end Get_Splittab;
   
   procedure Calc_Est is
   begin
      case Tax_Year is
         when 2002 | 2003 => Y2002.Calc_Est;
         when 2004 => Y2004.Calc_Est;
      end case;
   end Calc_Est;
   
end LibOpenSteuer.EstTab;
