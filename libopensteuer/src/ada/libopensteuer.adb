with GNAT.Float_Control;
with LibOpenSteuer.EstTab; use LibOpenSteuer.EstTab;
with LibOpenSteuer.LstTab; use LibOpenSteuer.LstTab;

package body LibOpenSteuer is
   
   procedure Einkommensteuer (Eingabe : in Est_Eingabe; Ausgabe : out Est_Ausgabe) is
   begin
      Berechne_Einkommensteuer (Eingabe, Ausgabe);
   end Einkommensteuer;
   
   procedure Lohnsteuer (Eingabe : in Lst_Eingabe; Ausgabe : out Lst_Ausgabe) is
   begin
      Berechne_Lohnsteuer (Eingabe, Ausgabe);
   end Lohnsteuer;
   
begin
   GNAT.Float_Control.Reset;
end LibOpenSteuer;
