with System;
with Interfaces.C; use Interfaces;

package LibOpenSteuer is

   --------------------------------------
   -- Die Zahlentypen für Berechnungen --
   --------------------------------------

   -- Maximale Float-Genauigkeit ist:
   OpenSteuer_Max_Digits : constant := System.Max_Base_Digits;

   -- Betrag in EUR ohne Nachkommastellen:
   type Euro is range System.Min_Int..System.Max_Int; -- new Integer;

   -- Betrag in Cent ohne Nachkommastellen:
   type Cent is range System.Min_Int..System.Max_Int; -- new Integer;

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

   -- Wrapper for access from C:
   procedure Est_Eingabe_Set_ZVE(Eingabe: in out Est_Eingabe;
                                 Zve: in C.int);
   -- Constructor for C:
   type Est_Eingabe_Access is access Est_Eingabe;
   function Est_Eingabe_New return Est_Eingabe_Access;


   type Est_Ausgabe is
   record
      Grund_Tab,
      Splitting_Tab : Euro;
   end record;

   -- Wrapper for access from C:
   function Est_Ausgabe_Get_Grund_Tab(Arg: in Est_Ausgabe)
                                      return C.int;
   function Est_Ausgabe_Get_Splitting_Tab(Arg: in Est_Ausgabe)
                                          return C.int;
   -- Constructor for C:
   type Est_Ausgabe_Access is access Est_Ausgabe;
   function Est_Ausgabe_New return Est_Ausgabe_Access;


   -- Calculate the actual income tax / Einkommensteuer
   procedure Einkommensteuer (Eingabe : in Est_Eingabe;
                              Ausgabe : out Est_Ausgabe);

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

   -- Wrapper for access from C:
   procedure Lst_Eingabe_Set_ALTER1(E: in out Lst_Eingabe; V : C.int);
   procedure Lst_Eingabe_Set_ZKF(E: in out Lst_Eingabe; V : C.double);
   procedure Lst_Eingabe_Set_HINZUR(E: in out Lst_Eingabe; V : C.int);
   procedure Lst_Eingabe_Set_JFREIB(E: in out Lst_Eingabe; V : C.int);
   procedure Lst_Eingabe_Set_JHINZU(E: in out Lst_Eingabe; V : C.int);
   procedure Lst_Eingabe_Set_JRE4(E: in out Lst_Eingabe; V : C.int);
   procedure Lst_Eingabe_Set_JVBEZ(E: in out Lst_Eingabe; V : C.int);
   procedure Lst_Eingabe_Set_SONSTB(E: in out Lst_Eingabe; V : C.int);
   procedure Lst_Eingabe_Set_VBEZ(E: in out Lst_Eingabe; V : C.int);
   procedure Lst_Eingabe_Set_VBS(E: in out Lst_Eingabe; V : C.int);
   procedure Lst_Eingabe_Set_VMT(E: in out Lst_Eingabe; V : C.int);
   procedure Lst_Eingabe_Set_WFUNDF(E: in out Lst_Eingabe; V : C.int);
   procedure Lst_Eingabe_Set_RE4(E: in out Lst_Eingabe; V : C.int);
   procedure Lst_Eingabe_Set_KRV(E: in out Lst_Eingabe; V : C.int);
   procedure Lst_Eingabe_Set_LZZ(E: in out Lst_Eingabe; V : C.int);
   procedure Lst_Eingabe_Set_STKL(E: in out Lst_Eingabe; V : C.int);

   type Lst_Eingabe_Access is access Lst_Eingabe;
   function Lst_Eingabe_New return Lst_Eingabe_Access;
   -- procedure Lst_Eingabe_Set_Alter1(Arg: in out Lst_Eingabe, alter1 : in C.int);

   type Lst_Ausgabe is
   record
      LSTLZZ,
      SOLZLZZ,
      K8,
      K9 : Cent;
   end record;

   -- Wrapper for access from C:
   function Lst_Ausgabe_Get_LSTLZZ(Arg: in Lst_Ausgabe) return C.int;
   function Lst_Ausgabe_Get_SOLZLZZ(Arg: in Lst_Ausgabe) return C.int;
   function Lst_Ausgabe_Get_K8(Arg: in Lst_Ausgabe) return C.int;
   function Lst_Ausgabe_Get_K9(Arg: in Lst_Ausgabe) return C.int;
   -- Constructor for C:
   type Lst_Ausgabe_Access is access Lst_Ausgabe;
   function Lst_Ausgabe_New return Lst_Ausgabe_Access;

   -- Calculate the actual salary tax / Lohnsteuer
   procedure Lohnsteuer (Eingabe : in Lst_Eingabe; Ausgabe : out Lst_Ausgabe);

   ---------------------------
   --  Export Declarations  --
   ---------------------------

   pragma Export (C, Est_Eingabe_New,
                  "LibOpenSteuer_Est_Eingabe_New");
   pragma Export (C, Est_Eingabe_Set_ZVE,
                  "LibOpenSteuer_Est_Eingabe_Set_ZVE");

   pragma Export (C, Est_Ausgabe_New,
                  "LibOpenSteuer_Est_Ausgabe_New");
   pragma Export (C, Est_Ausgabe_Get_Grund_Tab,
                  "LibOpenSteuer_Est_Ausgabe_Get_Grund_Tab");
   pragma Export (C, Est_Ausgabe_Get_Splitting_Tab,
                  "LibOpenSteuer_Est_Ausgabe_Get_Splitting_Tab");

   pragma Export (C, Einkommensteuer, "LibOpenSteuer_Einkommensteuer");

   pragma Export (C, Lst_Eingabe_Set_ALTER1, "LibOpenSteuer_Lst_Eingabe_Set_ALTER1");
   pragma Export (C, Lst_Eingabe_Set_ZKF, "LibOpenSteuer_Lst_Eingabe_Set_ZKF");
   pragma Export (C, Lst_Eingabe_Set_HINZUR, "LibOpenSteuer_Lst_Eingabe_Set_HINZUR");
   pragma Export (C, Lst_Eingabe_Set_JFREIB, "LibOpenSteuer_Lst_Eingabe_Set_JFREIB");
   pragma Export (C, Lst_Eingabe_Set_JHINZU, "LibOpenSteuer_Lst_Eingabe_Set_JHINZU");
   pragma Export (C, Lst_Eingabe_Set_JRE4, "LibOpenSteuer_Lst_Eingabe_Set_JRE4");
   pragma Export (C, Lst_Eingabe_Set_JVBEZ, "LibOpenSteuer_Lst_Eingabe_Set_JVBEZ");
   pragma Export (C, Lst_Eingabe_Set_SONSTB, "LibOpenSteuer_Lst_Eingabe_Set_SONSTB");
   pragma Export (C, Lst_Eingabe_Set_VBEZ, "LibOpenSteuer_Lst_Eingabe_Set_VBEZ");
   pragma Export (C, Lst_Eingabe_Set_VBS, "LibOpenSteuer_Lst_Eingabe_Set_VBS");
   pragma Export (C, Lst_Eingabe_Set_VMT, "LibOpenSteuer_Lst_Eingabe_Set_VMT");
   pragma Export (C, Lst_Eingabe_Set_WFUNDF, "LibOpenSteuer_Lst_Eingabe_Set_WFUNDF");
   pragma Export (C, Lst_Eingabe_Set_RE4, "LibOpenSteuer_Lst_Eingabe_Set_RE4");
   pragma Export (C, Lst_Eingabe_Set_KRV, "LibOpenSteuer_Lst_Eingabe_Set_KRV");
   pragma Export (C, Lst_Eingabe_Set_LZZ, "LibOpenSteuer_Lst_Eingabe_Set_LZZ");
   pragma Export (C, Lst_Eingabe_Set_STKL, "LibOpenSteuer_Lst_Eingabe_Set_STKL");
   pragma Export (C, Lst_Eingabe_New, "LibOpenSteuer_Lst_Eingabe_New");

   pragma Export (C, Lst_Ausgabe_Get_LSTLZZ, "LibOpenSteuer_Lst_Ausgabe_Get_LSTLZZ");
   pragma Export (C, Lst_Ausgabe_Get_SOLZLZZ, "LibOpenSteuer_Lst_Ausgabe_Get_SOLZLZZ");
   pragma Export (C, Lst_Ausgabe_Get_K8, "LibOpenSteuer_Lst_Ausgabe_Get_K8");
   pragma Export (C, Lst_Ausgabe_Get_K9, "LibOpenSteuer_Lst_Ausgabe_Get_K9");
   pragma Export (C, Lst_Ausgabe_New, "LibOpenSteuer_Lst_Ausgabe_New");

   pragma Export (C, Lohnsteuer, "LibOpenSteuer_Lohnsteuer");
   -- exporting procedures is easy. Concept copied from
   -- http://www.adapower.com/articles/howto-gdllc.html

end LibOpenSteuer;
