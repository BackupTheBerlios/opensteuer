with LibOpenSteuer.LstTab.Y2002;
with LibOpenSteuer.LstTab.Y2004;

package body LibOpenSteuer.LstTab is
   
   procedure Calc_Lst is
   begin
      case Tax_Year is
         when 2002 | 2003 => Y2002.Calc_Lst;
         when 2004 => Y2004.Calc_Lst;
      end case;
   end Calc_Lst;
   
end LibOpenSteuer.LstTab;
