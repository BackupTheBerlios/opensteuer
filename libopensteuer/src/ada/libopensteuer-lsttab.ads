--with LibOpenSteuer_Globals; use LibOpenSteuer_Globals;
--with LibOpenSteuer_PAP; use LibOpensteuer_PAP;

package LibOpenSteuer.LstTab is
   
   procedure Berechne_Lohnsteuer (Eingabe : in Lst_Eingabe; Ausgabe : out Lst_Ausgabe);
   
end LibOpenSteuer.LstTab;
