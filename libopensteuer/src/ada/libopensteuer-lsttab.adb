--with Ada.Strings.Fixed; use Ada.Strings.Fixed;
-- with Ada.Text_IO; use Ada.Text_IO;
with LibOpenSteuer.PAP; use LibOpenSteuer.PAP;
--with LibOpenSteuer_Globals; use LibOpenSteuer_Globals;
-- with OpenSteuer_Debug; use OpenSteuer_Debug;

package body LibOpenSteuer.LstTab is
   
   procedure Set_PAP_Eingabe (Eingabe : in Lst_Eingabe) is
   begin
      ALTER1 := Eingabe.ALTER1;
      ZKF := Eingabe.ZKF;
      HINZUR := Eingabe.HINZUR;
      JFREIB := Eingabe.JFREIB;
      JHINZU := Eingabe.JHINZU;
      JRE4 := Eingabe.JRE4;
      JVBEZ := Eingabe.JVBEZ;
      SONSTB := Eingabe.SONSTB;
      VBEZ := Eingabe.VBEZ;
      VBS := Eingabe.VBS;
      VMT := Eingabe.VMT;
      WFUNDF := Eingabe.WFUNDF;
      KRV := Eingabe.KRV;
      LZZ := Eingabe.LZZ;
      RE4 := Eingabe.RE4;
      STKL := Eingabe.STKL;
   end Set_PAP_Eingabe;
   
   procedure Berechne_Lohnsteuer (Eingabe : in Lst_Eingabe; Ausgabe : out Lst_Ausgabe) is
   begin
      Reset_PAP_Intern;
      Reset_PAP_Eingabe;
      Set_PAP_Eingabe (Eingabe);
      LST2002;
      Ausgabe.LSTLZZ := LSTLZZ;
      Ausgabe.SOLZLZZ := SOLZLZZ;
      Ausgabe.K8 := Cent (RW_Intern (LSTLZZ) * 8.0 / 100.0);
      Ausgabe.K9 := Cent (RW_Intern (LSTLZZ) * 9.0 / 100.0);
   end Berechne_Lohnsteuer;
   
end LibOpenSteuer.LstTab;
