with System;

package LibOpenSteuer_Globals is
   
   --------------------------------------
   -- Die Zahlentypen für Berechnungen --
   --------------------------------------
   
   -- Maximale Float-Genauigkeit ist:
   OpenSteuer_Max_Digits : constant := System.Max_Base_Digits;

   -- der PAP verlangt insgesamt sechs Typen:
   
   -- 1) Betrag in EUR ohne Nachkommastellen:
   type Euro is range System.Min_Int..System.Max_Int;
   
   -- 2) Betrag in Cent ohne Nachkommastellen:
   type Cent is range System.Min_Int..System.Max_Int;
   
   -- 3) Rechenwert mit 1 Nachkommastelle:
   type RW_1 is delta 0.1 digits OpenSteuer_Max_Digits;
   
   -- 4) Rechenwert mit 2 Nachkommastellen:
   type RW_2 is delta 0.01 digits OpenSteuer_Max_Digits;
   
   -- 5) Rechenwert mit 3 Nachkommastellen:
   type RW_3 is delta 0.001 digits OpenSteuer_Max_Digits;
   
   -- 6) Rechenwert mit 5 Nachkommastellen:
   type RW_5 is delta 0.00001 digits OpenSteuer_Max_Digits;
   
   -- Intern rechnen wir mit max. Genauigkeit:
   type RW_Intern is digits OpenSteuer_Max_Digits;
   
   -- der PAP verlangt, intern mit dreistelliger Genauigkeit
   -- zu rechnen. Scheint aber falsche Ergebnisse zu bringen:
   -- subtype RW_Intern is RW_3;
   
   ------------------------------------------------------------------
   -- Im PAP verwendete Typen, die auch extern bekannt sein sollen --
   ------------------------------------------------------------------
   
   -- Ich verwende aus Gründen der Einfachheit Subtypen von Integer,
   -- weil sonst zu viele Umwandlungen nötig wären, ohne dass es einen
   -- im Verhältnis stehenden Vorteil hätte.
   subtype ALTER1_Type is Natural Range 0..1;
   subtype KRV_Type is Natural Range 0..1;
   subtype LZZ_Type is Positive Range 1..4;
   subtype STKL_Type is Positive Range 1..6;
   subtype R_Type is Natural Range 0..1;
   subtype KZTAB_Type is Natural Range 1..2;
   
   -----------------------
   -- Globale Variablen --
   -----------------------
   
   Steuer_Jahr : Positive := 2002;
   
end LibOpenSteuer_Globals;
