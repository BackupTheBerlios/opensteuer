with LibOpenSteuer.PAP; use LibOpenSteuer.PAP;

package body LibOpenSteuer.LstTab.Y2002 is
   
   procedure Calc_Lst is
   begin
      Reset_PAP_Intern;
      PAP.LstTab;
   end Calc_Lst;
   
end LibOpenSteuer.LstTab.Y2002;
