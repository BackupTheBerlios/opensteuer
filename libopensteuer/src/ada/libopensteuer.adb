with GNAT.Float_Control;
with LibOpenSteuer.EstTab; use LibOpenSteuer.EstTab;
with LibOpenSteuer.LstTab; use LibOpenSteuer.LstTab;
with Interfaces.C;

package body LibOpenSteuer is

   procedure Einkommensteuer (Eingabe : in Est_Eingabe; Ausgabe : out Est_Ausgabe) is
   begin
      Berechne_Einkommensteuer (Eingabe, Ausgabe);
   end Einkommensteuer;

   procedure Lohnsteuer (Eingabe : in Lst_Eingabe; Ausgabe : out Lst_Ausgabe) is
   begin
      Berechne_Lohnsteuer (Eingabe, Ausgabe);
   end Lohnsteuer;

   --
   -- Wrapper for Est_Eingabe
   --

   function Est_Eingabe_New return Est_Eingabe_Access is
   begin
      return new Est_Eingabe;
   end Est_Eingabe_New;

   procedure Est_Eingabe_Set_ZVE(Eingabe: in out Est_Eingabe;
                                 Zve: in C.int) is
   begin
      -- Ada.Text_IO.put_line("Set_ZVE: trying to set " & Zve'img);
      -- Ada.Text_IO.put_line("Set_ZVE: so far, ZVE is " & Eingabe.ZVE'img);
      Eingabe.ZVE := Euro(Zve);
   end Est_Eingabe_Set_ZVE;

   --
   -- Wrapper for Est_Ausgabe
   --

   function Est_Ausgabe_New return Est_Ausgabe_Access is
   begin
        return new Est_Ausgabe;
   end Est_Ausgabe_New;

   function Est_Ausgabe_Get_Grund_Tab(Arg: in Est_Ausgabe)
                                      return C.int is
   begin
      return C.int(Arg.Grund_Tab);
   end Est_Ausgabe_Get_Grund_Tab;

   Function Est_Ausgabe_Get_Splitting_Tab(Arg: in Est_Ausgabe)
                                          return C.int is
   begin
      return C.int(Arg.Splitting_Tab);
   end Est_Ausgabe_Get_Splitting_Tab;

   --
   -- Wrapper for Lst_Eingabe
   --

   procedure Lst_Eingabe_Set_ALTER1(E: in out Lst_Eingabe; V : C.int) is
   begin
      E.ALTER1 := ALTER1_Type(V);
   end Lst_Eingabe_Set_ALTER1;
   procedure Lst_Eingabe_Set_ZKF(E: in out Lst_Eingabe; V : C.double) is
   begin
      E.ZKF := ZKF_Type(V);
   end Lst_Eingabe_Set_ZKF;
   procedure Lst_Eingabe_Set_HINZUR(E: in out Lst_Eingabe; V : C.int) is
   begin
      E.HINZUR := Cent(V);
   end Lst_Eingabe_Set_HINZUR;
   procedure Lst_Eingabe_Set_JFREIB(E: in out Lst_Eingabe; V : C.int) is
   begin
      E.JFREIB := Cent(V);
   end Lst_Eingabe_Set_JFREIB;
   procedure Lst_Eingabe_Set_JHINZU(E: in out Lst_Eingabe; V : C.int) is
   begin
      E.JHINZU := Cent(V);
   end Lst_Eingabe_Set_JHINZU;
   procedure Lst_Eingabe_Set_JRE4(E: in out Lst_Eingabe; V : C.int) is
   begin
      E.JRE4 := Cent(V);
   end Lst_Eingabe_Set_JRE4;
   procedure Lst_Eingabe_Set_JVBEZ(E: in out Lst_Eingabe; V : C.int) is
   begin
      E.JVBEZ := Cent(V);
   end Lst_Eingabe_Set_JVBEZ;
   procedure Lst_Eingabe_Set_SONSTB(E: in out Lst_Eingabe; V : C.int) is
   begin
      E.SONSTB := Cent(V);
   end Lst_Eingabe_Set_SONSTB;
   procedure Lst_Eingabe_Set_VBEZ(E: in out Lst_Eingabe; V : C.int) is
   begin
      E.VBEZ := Cent(V);
   end Lst_Eingabe_Set_VBEZ;
   procedure Lst_Eingabe_Set_VBS(E: in out Lst_Eingabe; V : C.int) is
   begin
      E.VBS := Cent(V);
   end Lst_Eingabe_Set_VBS;
   procedure Lst_Eingabe_Set_VMT(E: in out Lst_Eingabe; V : C.int) is
   begin
      E.VMT := Cent(V);
   end Lst_Eingabe_Set_VMT;
   procedure Lst_Eingabe_Set_WFUNDF(E: in out Lst_Eingabe; V : C.int) is
   begin
      E.WFUNDF := Cent(V);
   end Lst_Eingabe_Set_WFUNDF;
   procedure Lst_Eingabe_Set_RE4(E: in out Lst_Eingabe; V : C.int) is
   begin
      E.RE4 := Cent(V);
   end Lst_Eingabe_Set_RE4;
   procedure Lst_Eingabe_Set_KRV(E: in out Lst_Eingabe; V : C.int) is
   begin
      E.KRV := KRV_Type(V);
   end Lst_Eingabe_Set_KRV;
   procedure Lst_Eingabe_Set_LZZ(E: in out Lst_Eingabe; V : C.int) is
   begin
      E.LZZ := LZZ_Type(V);
   end Lst_Eingabe_Set_LZZ;
   procedure Lst_Eingabe_Set_STKL(E: in out Lst_Eingabe; V : C.int) is
   begin
      E.STKL := STKL_Type(V);
   end Lst_Eingabe_Set_STKL;

   function Lst_Eingabe_New return Lst_Eingabe_Access is
   begin
      return new Lst_Eingabe;
   end Lst_Eingabe_New;

   --
   -- Wrapper for Lst_Ausgabe
   --

   function Lst_Ausgabe_Get_LSTLZZ(Arg: in Lst_Ausgabe) return C.int is
   begin
        return C.int(Arg.LSTLZZ);
   end Lst_Ausgabe_Get_LSTLZZ;
   function Lst_Ausgabe_Get_SOLZLZZ(Arg: in Lst_Ausgabe) return C.int is
   begin
        return C.int(Arg.SOLZLZZ);
   end Lst_Ausgabe_Get_SOLZLZZ;
   function Lst_Ausgabe_Get_K8(Arg: in Lst_Ausgabe) return C.int is
   begin
        return C.int(Arg.K8);
   end Lst_Ausgabe_Get_K8;
   function Lst_Ausgabe_Get_K9(Arg: in Lst_Ausgabe) return C.int is
   begin
        return C.int(Arg.K9);
   end Lst_Ausgabe_Get_K9;
   function Lst_Ausgabe_New return Lst_Ausgabe_Access is
   begin
      return new Lst_Ausgabe;
   end Lst_Ausgabe_New;


begin
   GNAT.Float_Control.Reset;
end LibOpenSteuer;
