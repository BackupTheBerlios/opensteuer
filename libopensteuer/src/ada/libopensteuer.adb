with GNAT.Float_Control;
with Ada.Exceptions; use Ada.Exceptions;
with LibOpenSteuer.EstTab;
with LibOpenSteuer.LstTab;
with LibOpenSteuer.PAP;

package body LibOpenSteuer is
   
   -----------------
   -- Error-Codes --
   -----------------
   
   function Get_Error_Code (E : Exception_Occurrence) return General_Type_C is
   begin
      if Exception_Identity (E) = Program_Error'Identity then
         return Program_Error_Code;
      elsif Exception_Identity (E) = Tasking_Error'Identity then
         return Tasking_Error_Code;
      elsif Exception_Identity (E) = Storage_Error'Identity then
         return Storage_Error_Code;
      elsif Exception_Identity (E) = Constraint_Error'Identity then
         return Constraint_Error_Code;
      elsif Exception_Identity (E) = Parameter_Not_Defined_Error'Identity then
         return Parameter_Not_Defined_Error_Code;
      else
         return General_Error_Code;
      end if;
   end Get_Error_Code;
   
   ----------
   -- Year --
   ----------
   
   function Get_First_Year return General_Type is
   begin
      return General_Type (Year_Type'First);
   end Get_First_Year;
   
   function Get_First_Year_C return General_Type_C is
   begin
      return General_Type_C (Year_Type'First);
   exception
            when E : others => return Get_Error_Code (E);
   end Get_First_Year_C;
   
   function Get_Last_Year return General_Type is
   begin
      return General_Type (Year_Type'Last);
   end Get_Last_Year;
   
   function Get_Last_Year_C return General_Type_C is
   begin
      return General_Type_C (Year_Type'Last);
   exception
            when E : others => return Get_Error_Code (E);
   end Get_Last_Year_C;
   
   procedure Set_Year (Year : General_Type) is
   begin
      Tax_Year := Year_Type (Year);
   end Set_Year;
   
   function Set_Year_C (Year : General_Type_C) return General_Type_C is
   begin
      Tax_Year := Year_Type (Year);
      return No_Error_Code;
   exception
            when E : others => return Get_Error_Code (E);
   end Set_Year_C;
   
   ---------
   -- Set --
   ---------
   
   package P renames LibOpenSteuer.PAP;
   
   procedure Set_ALTER1 (ALTER1 : in General_Type) is
   begin
      P.Set_ALTER1 (ALTER1);
   end Set_ALTER1;
   
   function Set_ALTER1_C (ALTER1 : in General_Type_C) return General_Type_C is
   begin
      P.Set_ALTER1 (General_Type (ALTER1));
      return No_Error_Code;
   exception
            when E : others => return Get_Error_Code (E);
   end Set_ALTER1_C;
   
   procedure Set_HINZUR (HINZUR : in Cent_Type) is
   begin
      P.Set_HINZUR (Cent_Type (HINZUR));
   end Set_HINZUR;
   
   function Set_HINZUR_C (HINZUR : in Cent_Type_C) return General_Type_C is
   begin
      P.Set_HINZUR (Cent_Type (HINZUR));
      return No_Error_Code;
   exception
            when E : others => return Get_Error_Code (E);
   end Set_HINZUR_C;
   
   procedure Set_JFREIB (JFREIB : in Cent_Type) is
   begin
      P.Set_JFREIB (Cent_Type (JFREIB));
   end Set_JFREIB;
   
   function Set_JFREIB_C (JFREIB : in Cent_Type_C) return General_Type_C is
   begin
      P.Set_JFREIB (Cent_Type (JFREIB));
      return No_Error_Code;
   exception
            when E : others => return Get_Error_Code (E);
   end Set_JFREIB_C;
   
   procedure Set_JHINZU (JHINZU : in Cent_Type) is
   begin
      P.Set_JHINZU (Cent_Type (JHINZU));
   end Set_JHINZU;
   
   function Set_JHINZU_C (JHINZU : in Cent_Type_C) return General_Type_C is
   begin
      P.Set_JHINZU (Cent_Type (JHINZU));
      return No_Error_Code;
   exception
            when E : others => return Get_Error_Code (E);
   end Set_JHINZU_C;
   
   procedure Set_JRE4 (JRE4 : in Cent_Type) is
   begin
      P.Set_JRE4 (Cent_Type (JRE4));
   end Set_JRE4;
   
   function Set_JRE4_C (JRE4 : in Cent_Type_C) return General_Type_C is
   begin
      P.Set_JRE4 (Cent_Type (JRE4));
      return No_Error_Code;
   exception
            when E : others => return Get_Error_Code (E);
   end Set_JRE4_C;
   
   procedure Set_JVBEZ (JVBEZ : in Cent_Type) is
   begin
      P.Set_JVBEZ (Cent_Type (JVBEZ));
   end Set_JVBEZ;
   
   function Set_JVBEZ_C (JVBEZ : in Cent_Type_C) return General_Type_C is
   begin
      P.Set_JVBEZ (Cent_Type (JVBEZ));
      return No_Error_Code;
   exception
            when E : others => return Get_Error_Code (E);
   end Set_JVBEZ_C;
   
   procedure Set_KRV (KRV : in General_Type) is
   begin
      P.Set_KRV (KRV);
   end Set_KRV;
   
   function Set_KRV_C (KRV : in General_Type_C) return General_Type_C is
   begin
      P.Set_KRV (General_Type (KRV));
      return No_Error_Code;
   exception
            when E : others => return Get_Error_Code (E);
   end Set_KRV_C;
   
   procedure Set_LZZ (LZZ : in General_Type) is
   begin
      P.Set_LZZ (LZZ);
   end Set_LZZ;
   
   function Set_LZZ_C (LZZ : in General_Type_C) return General_Type_C is
   begin
      P.Set_LZZ (General_Type (LZZ));
      return No_Error_Code;
   exception
            when E : others => return Get_Error_Code (E);
   end Set_LZZ_C;
   
   procedure Set_R (R : in General_Type) is
   begin
      P.Set_R (R);
   end Set_R;
   
   function Set_R_C (R : in General_Type_C) return General_Type_C is
   begin
      P.Set_R (General_Type (R));
      return No_Error_Code;
   exception
            when E : others => return Get_Error_Code (E);
   end Set_R_C;
   
   procedure Set_RE4 (RE4 : in Cent_Type) is
   begin
      P.Set_RE4 (Cent_Type (RE4));
   end Set_RE4;
   
   function Set_RE4_C (RE4 : in Cent_Type_C) return General_Type_C is
   begin
      P.Set_RE4 (Cent_Type (RE4));
      return No_Error_Code;
   exception
            when E : others => return Get_Error_Code (E);
   end Set_RE4_C;
   
   procedure Set_SONSTB (SONSTB : in Cent_Type) is
   begin
      P.Set_SONSTB (Cent_Type (SONSTB));
   end Set_SONSTB;
   
   function Set_SONSTB_C (SONSTB : in Cent_Type_C) return General_Type_C is
   begin
      P.Set_SONSTB (Cent_Type (SONSTB));
      return No_Error_Code;
   exception
            when E : others => return Get_Error_Code (E);
   end Set_SONSTB_C;
   
   procedure Set_STKL (STKL : in General_Type) is
   begin
      P.Set_STKL (STKL);
   end Set_STKL;
   
   function Set_STKL_C (STKL : in General_Type_C) return General_Type_C is
   begin
      P.Set_STKL (General_Type (STKL));
      return No_Error_Code;
   exception
            when E : others => return Get_Error_Code (E);
   end Set_STKL_C;
   
   procedure Set_VBEZ (VBEZ : in Cent_Type) is
   begin
      P.Set_VBEZ (Cent_Type (VBEZ));
   end Set_VBEZ;
   
   function Set_VBEZ_C (VBEZ : in Cent_Type_C) return General_Type_C is
   begin
      P.Set_VBEZ (Cent_Type (VBEZ));
      return No_Error_Code;
   exception
            when E : others => return Get_Error_Code (E);
   end Set_VBEZ_C;
   
   procedure Set_VBS (VBS : in Cent_Type) is
   begin
      P.Set_VBS (Cent_Type (VBS));
   end Set_VBS;
   
   function Set_VBS_C (VBS : in Cent_Type_C) return General_Type_C is
   begin
      P.Set_VBS (Cent_Type (VBS));
      return No_Error_Code;
   exception
            when E : others => return Get_Error_Code (E);
   end Set_VBS_C;
   
   procedure Set_VMT (VMT : in Cent_Type) is
   begin
      P.Set_VMT (Cent_Type (VMT));
   end Set_VMT;
   
   function Set_VMT_C (VMT : in Cent_Type_C) return General_Type_C is
   begin
      P.Set_VMT (Cent_Type (VMT));
      return No_Error_Code;
   exception
            when E : others => return Get_Error_Code (E);
   end Set_VMT_C;
   
   procedure Set_WFUNDF (WFUNDF : in Cent_Type) is
   begin
      P.Set_WFUNDF (Cent_Type (WFUNDF));
   end Set_WFUNDF;
   
   function Set_WFUNDF_C (WFUNDF : in Cent_Type_C) return General_Type_C is
   begin
      P.Set_WFUNDF (Cent_Type (WFUNDF));
      return No_Error_Code;
   exception
            when E : others => return Get_Error_Code (E);
   end Set_WFUNDF_C;
   
   procedure Set_ZKF (ZKF : in General_Type) is
   begin
      P.Set_ZKF (ZKF);
   end Set_ZKF;
   
   function Set_ZKF_C (ZKF : in General_Type_C) return General_Type_C is
   begin
      P.Set_ZKF (General_Type (ZKF));
      return No_Error_Code;
   exception
            when E : others => return Get_Error_Code (E);
   end Set_ZKF_C;
   
   ---------
   -- Get --
   ---------
   
   function Get_BK return Cent_Type is
   begin
      return P.Get_BK;
   end Get_BK;
   
   function Get_BK_C return Cent_Type_C is
   begin
      return Cent_Type_C (P.Get_BK);
   exception
            when E : others => return Cent_Type_C (Get_Error_Code (E));
   end Get_BK_C;
   
   function Get_BKS return Cent_Type is
   begin
      return P.Get_BKS;
   end Get_BKS;
   
   function Get_BKS_C return Cent_Type_C is
   begin
      return Cent_Type_C (P.Get_BKS);
   exception
            when E : others => return Cent_Type_C (Get_Error_Code (E));
   end Get_BKS_C;
   
   function Get_BKV return Cent_Type is
   begin
      return P.Get_BKV;
   end Get_BKV;
   
   function Get_BKV_C return Cent_Type_C is
   begin
      return Cent_Type_C (P.Get_BKV);
   exception
            when E : others => return Cent_Type_C (Get_Error_Code (E));
   end Get_BKV_C;
   
   function Get_LSTLZZ return Cent_Type is
   begin
      return P.Get_LSTLZZ;
   end Get_LSTLZZ;
   
   function Get_LSTLZZ_C return Cent_Type_C is
   begin
      return Cent_Type_C (P.Get_LSTLZZ);
   exception
            when E : others => return Cent_Type_C (Get_Error_Code (E));
   end Get_LSTLZZ_C;
   
   function Get_LZALOG return Cent_Type is
   begin
      return P.Get_LZALOG;
   end Get_LZALOG;
   
   function Get_LZALOG_C return Cent_Type_C is
   begin
      return Cent_Type_C (P.Get_LZALOG);
   exception
            when E : others => return Cent_Type_C (Get_Error_Code (E));
   end Get_LZALOG_C;
   
   function Get_LZALUG return Cent_Type is
   begin
      return P.Get_LZALUG;
   end Get_LZALUG;
   
   function Get_LZALUG_C return Cent_Type_C is
   begin
      return Cent_Type_C (P.Get_LZALUG);
   exception
            when E : others => return Cent_Type_C (Get_Error_Code (E));
   end Get_LZALUG_C;
   
   function Get_SOLZLZZ return Cent_Type is
   begin
      return P.Get_SOLZLZZ;
   end Get_SOLZLZZ;
   
   function Get_SOLZLZZ_C return Cent_Type_C is
   begin
      return Cent_Type_C (P.Get_SOLZLZZ);
   exception
            when E : others => return Cent_Type_C (Get_Error_Code (E));
   end Get_SOLZLZZ_C;
   
   function Get_SOLZS return Cent_Type is
   begin
      return P.Get_SOLZS;
   end Get_SOLZS;
   
   function Get_SOLZS_C return Cent_Type_C is
   begin
      return Cent_Type_C (P.Get_SOLZS);
   exception
            when E : others => return Cent_Type_C (Get_Error_Code (E));
   end Get_SOLZS_C;
   
   function Get_SOLZV return Cent_Type is
   begin
      return P.Get_SOLZV;
   end Get_SOLZV;
   
   function Get_SOLZV_C return Cent_Type_C is
   begin
      return Cent_Type_C (P.Get_SOLZV);
   exception
            when E : others => return Cent_Type_C (Get_Error_Code (E));
   end Get_SOLZV_C;
   
   function Get_STS return Cent_Type is
   begin
      return P.Get_STS;
   end Get_STS;
   
   function Get_STS_C return Cent_Type_C is
   begin
      return Cent_Type_C (P.Get_STS);
   exception
            when E : others => return Cent_Type_C (Get_Error_Code (E));
   end Get_STS_C;
   
   function Get_STV return Cent_Type is
   begin
      return P.Get_STV;
   end Get_STV;
   
   function Get_STV_C return Cent_Type_C is
   begin
      return Cent_Type_C (P.Get_STV);
   exception
            when E : others => return Cent_Type_C (Get_Error_Code (E));
   end Get_STV_C;
   
   ---------------------
   -- Einkommensteuer --
   ---------------------
   
   package E renames LibOpenSteuer.EstTab;
   
   -- Don't call P.Set_ZVE to set ZVE, because ZVE is an internal Parameter
   -- which is overwritten prior to every run.  Use E.Set_ZVE instead which
   -- makes a local copy.  Calc_Est will call P.Set_ZVE then.
   
   procedure Set_ZVE (ZVE : in Cent_Type) is
   begin
      E.Set_ZVE (Cent_Type (ZVE));
   end Set_ZVE;
   
   function Set_ZVE_C (ZVE : in Cent_Type_C) return General_Type_C is
   begin
      E.Set_ZVE (Cent_Type (ZVE));
      return No_Error_Code;
   exception
            when E : others => return Get_Error_Code (E);
   end Set_ZVE_C;
   
   function Get_Grundtab return Cent_Type is
   begin
      return E.Get_Grundtab;
   end Get_Grundtab;
   
   function Get_Grundtab_C return Cent_Type_C is
   begin
      return Cent_Type_C (E.Get_Grundtab);
   exception
            when E : others => return Cent_Type_C (Get_Error_Code (E));
   end Get_Grundtab_C;
   
   function Get_Splittab return Cent_Type is
   begin
      return E.Get_Splittab;
   end Get_Splittab;
   
   function Get_Splittab_C return Cent_Type_C is
   begin
      return Cent_Type_C (E.Get_Splittab);
   exception
            when E : others => return Cent_Type_C (Get_Error_Code (E));
   end Get_Splittab_C;
   
   procedure Calc_Est is
   begin
      E.Calc_Est;
   end Calc_Est;
   
   function Calc_Est_C return General_Type_C is
   begin
      E.Calc_Est;
      return No_Error_Code;
   exception
            when E : others => return Get_Error_Code (E);
   end Calc_Est_C;
   
   ----------------
   -- Lohnsteuer --
   ----------------
   
   package L renames LibOpenSteuer.LstTab;
   
   procedure Calc_Lst is
   begin
      L.Calc_Lst;
   end Calc_Lst;
   
   function Calc_Lst_C return General_Type_C is
   begin
      L.Calc_Lst;
      return No_Error_Code;
   exception
            when E : others => return Get_Error_Code (E);
   end Calc_Lst_C;
   
   -------------------
   -- Rechenbereich --
   -------------------
   
   function Get_Min_Amount return Cent_Type is
   begin
      return P.Get_Min_Amount;
   end Get_Min_Amount;
   
   function Get_Min_Amount_C return Cent_Type_C is
   begin
      return Cent_Type_C (P.Get_Min_Amount);
   exception
            when E : others => return Cent_Type_C (Get_Error_Code (E));
   end Get_Min_Amount_C;
   
   function Get_Max_Amount return Cent_Type is
   begin
      return P.Get_Max_Amount;
   end Get_Max_Amount;
   
   function Get_Max_Amount_C return Cent_Type_C is
   begin
      return Cent_Type_C (P.Get_Max_Amount);
   exception
            when E : others => return Cent_Type_C (Get_Error_Code (E));
   end Get_Max_Amount_C;
   
   ---------------------
   -- Reset-functions --
   ---------------------
   
   procedure Reset_All is
   begin
      P.Reset_PAP_Eingabe;
      P.Reset_PAP_Intern;
   end Reset_All;
   
   function Reset_All_C return General_Type_C is
   begin
      P.Reset_PAP_Eingabe;
      P.Reset_PAP_Intern;
      return No_Error_Code;
   exception
            when E : others => return Get_Error_Code (E);
   end Reset_All_C;
   
   begin
      GNAT.Float_Control.Reset;
end LibOpenSteuer;
