with LibOpenSteuer.PAP; use LibOpenSteuer.PAP;
--with LibOpenSteuer_Globals; use LibOpenSteuer_Globals;

package body LibOpenSteuer.EstTab is
   
   procedure Berechne_Einkommensteuer (Eingabe : in Est_Eingabe; Ausgabe : out Est_Ausgabe) is
   begin
      Reset_PAP_Intern;
      Reset_PAP_Eingabe;
      STKL := 1;
      KZTAB := 1;
      ZVE := Eingabe.ZVE;
      MLSTJAHR_2;
      Ausgabe.Grund_Tab := ST;
      
      Reset_PAP_Intern;
      STKL := 1;
      KZTAB := 2;
      ZVE := Eingabe.ZVE;
      MLSTJAHR_2;
      Ausgabe.Splitting_Tab := ST;
   end Berechne_Einkommensteuer;
   
end LibOpenSteuer.EstTab;
