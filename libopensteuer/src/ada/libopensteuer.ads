with System;

package LibOpenSteuer is
   
   --------------------------------------
   -- Die Zahlentypen für Berechnungen --
   --------------------------------------
   
   -- Maximale Float-Genauigkeit ist:
   OpenSteuer_Max_Digits : constant := System.Max_Base_Digits;
   
   -- Betrag in EUR ohne Nachkommastellen:
   type Euro is range System.Min_Int..System.Max_Int;
   
   -- Betrag in Cent ohne Nachkommastellen:
   type Cent is range System.Min_Int..System.Max_Int;
   
   -- Rechenwert mit 1 Nachkommastelle:
   type RW_1 is delta 0.1 digits OpenSteuer_Max_Digits;
   
   ------------------------------------------------------------------
   -- Im PAP verwendete Typen, die auch extern bekannt sein müssen --
   ------------------------------------------------------------------
   
   subtype ALTER1_Type is Natural Range 0..1;
   subtype KRV_Type is Natural Range 0..1;
   subtype LZZ_Type is Positive Range 1..4;
   subtype STKL_Type is Positive Range 1..6;
   subtype R_Type is Natural Range 0..1;
   subtype KZTAB_Type is Natural Range 1..2;
   subtype ZKF_Type is RW_1 Range 0.0..9.5;
   
   -----------------------
   -- Globale Variablen --
   -----------------------
   
   Steuer_Jahr : Positive := 2002;
   
   ---------------------
   -- Einkommensteuer --
   ---------------------
   
   type Est_Eingabe is
   record
      ZVE : Euro := 0;
   end record;
   
   type Est_Ausgabe is
   record
      Grund_Tab,
      Splitting_Tab : Euro;
   end record;
   
   procedure Einkommensteuer (Eingabe : in Est_Eingabe; Ausgabe : out Est_Ausgabe);
   
   ----------------
   -- Lohnsteuer --
   ----------------
   
   type Lst_Eingabe is
   record
      ALTER1 : ALTER1_Type := 0;
      ZKF : ZKF_Type := 0.0;
      HINZUR,
      JFREIB,
      JHINZU,
      JRE4,
      JVBEZ,
      SONSTB,
      VBEZ,
      VBS,
      VMT,
      WFUNDF,
      RE4 : Cent := 0;
      KRV : KRV_Type := 0;
      LZZ : LZZ_Type := 2;
      STKL : STKL_Type := 1;
   end record;
   
   type Lst_Ausgabe is
   record
      LSTLZZ,
      SOLZLZZ,
      K8,
      K9 : Cent;
   end record;
   
   procedure Lohnsteuer (Eingabe : in Lst_Eingabe; Ausgabe : out Lst_Ausgabe);
   
end LibOpenSteuer;
