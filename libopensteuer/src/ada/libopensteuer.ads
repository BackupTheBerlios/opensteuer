with System;
with Interfaces.C;

package LibOpenSteuer is
   
   --------------
   -- IO-Types --
   --------------
   
   -- General_Type is a type for universal input/output beside currency
   -- amounts.  Examples would be the tax-class or the tax-year.  We need a
   -- universal type here, because we don't know at this stage which range
   -- the concrete type in a certain year has.
   
   type General_Type is range 0 .. 2500;
   
   -- Cent_Type is a type for input and output of currency amounts.
   -- No negative amounts are allowed.  Currency is Euro-Cents.
   
   type Cent_Type is range 0 .. System.Max_Int;
   
   -- General_Type_C and Cent_Type_C are the counterparts of the Ada-types
   -- for the C-part of the interface.  Both types must allow negative values,
   -- because we also use them to return error-codes.
   
   type General_Type_C is new Interfaces.C.int;
   type Cent_Type_C is new Interfaces.C.long;
   
   --------------------------------
   -- Exceptions and Error-Codes --
   --------------------------------
   
   -- We need the error-codes as return-values, because C cannot deal with
   -- Ada-exceptions.
   
   -- IMPORTANT NOTE: There is no guarantee that the values of the
   -- errors-codes will stay constant.  But the names will, hopefully.
   -- So, use the names instead of the values if possible.
   
   -- No error:
   
   No_Error_Code : constant General_Type_C := 0;
   pragma export (C, No_Error_Code, "no_error_code");
   
   -- Fallback:
   
   General_Error_Code : constant General_Type_C := -1;
   pragma export (C, General_Error_Code, "general_error_code");
   
   -- Counterparts to standard Ada-exceptions:
   
   Program_Error_Code : constant General_Type_C := -2;
   pragma export (C, Program_Error_Code, "program_error_code");
   
   Tasking_Error_Code : constant General_Type_C := -3;
   pragma export (C, Tasking_Error_Code, "tasking_error_code");
   
   Storage_Error_Code : constant General_Type_C := -4;
   pragma export (C, Storage_Error_Code, "storage_error_code");
   
   Constraint_Error_Code : constant General_Type_C := -5;
   pragma export (C, Constraint_Error_Code, "constraint_error_code");
   
   -- Our Exceptions:
   
   Parameter_Not_Defined_Error : exception;
   Parameter_Not_Defined_Error_Code : constant General_Type_C := -20;
   pragma export (C, Parameter_Not_Defined_Error_Code, "parameter_not_defined_error_code");
   
   ------------------
   -- Set the year --
   ------------------
   
   function Get_First_Year return General_Type;
   function Get_First_Year_C return General_Type_C;
   pragma export (C, Get_First_Year_C, "get_first_year");
   
   function Get_Last_Year return General_Type;
   function Get_Last_Year_C return General_Type_C;
   pragma export (C, Get_Last_Year_C, "get_last_year");
   
   procedure Set_Year (Year : in General_Type);
   function Set_Year_C (Year : in General_Type_C) return General_Type_C;
   pragma export (C, Set_Year_C, "set_year");
   
   ------------------------
   -- Set PAP-parameters --
   ------------------------
   
   procedure Set_ALTER1 (ALTER1 : in General_Type);
   function Set_ALTER1_C (ALTER1 : in General_Type_C) return General_Type_C;
   pragma export (C, Set_ALTER1_C, "set_alter1");
   
   procedure Set_HINZUR (HINZUR : in Cent_Type);
   function Set_HINZUR_C (HINZUR : in Cent_Type_C) return General_Type_C;
   pragma export (C, Set_HINZUR_C, "set_hinzur");
   
   procedure Set_JFREIB (JFREIB : in Cent_Type);
   function Set_JFREIB_C (JFREIB : in Cent_Type_C) return General_Type_C;
   pragma export (C, Set_JFREIB_C, "set_jfreib");
   
   procedure Set_JHINZU (JHINZU : in Cent_Type);
   function Set_JHINZU_C (JHINZU : in Cent_Type_C) return General_Type_C;
   pragma export (C, Set_JHINZU_C, "set_jhinzu");
   
   procedure Set_JRE4 (JRE4: in Cent_Type);
   function Set_JRE4_C (JRE4: in Cent_Type_C) return General_Type_C;
   pragma export (C, Set_JRE4_C, "set_jre4");
   
   procedure Set_JVBEZ (JVBEZ: in Cent_Type);
   function Set_JVBEZ_C (JVBEZ: in Cent_Type_C) return General_Type_C;
   pragma export (C, Set_JVBEZ_C, "set_jvbez");
   
   procedure Set_KRV (KRV : in General_Type);
   function Set_KRV_C (KRV : in General_Type_C) return General_Type_C;
   pragma export (C, Set_KRV_C, "set_krv");
   
   procedure Set_LZZ (LZZ : in General_Type);
   function Set_LZZ_C (LZZ : in General_Type_C) return General_Type_C;
   pragma export (C, Set_LZZ_C, "set_lzz");
   
   procedure Set_R (R : in General_Type);
   function Set_R_C (R : in General_Type_C) return General_Type_C;
   pragma export (C, Set_R_C, "set_r");
   
   procedure Set_RE4 (RE4: in Cent_Type);
   function Set_RE4_C (RE4: in Cent_Type_C) return General_Type_C;
   pragma export (C, Set_RE4_C, "set_re4");
   
   procedure Set_SONSTB (SONSTB: in Cent_Type);
   function Set_SONSTB_C (SONSTB: in Cent_Type_C) return General_Type_C;
   pragma export (C, Set_SONSTB_C, "set_sonstb");
   
   procedure Set_STKL (STKL : in General_Type);
   function Set_STKL_C (STKL : in General_Type_C) return General_Type_C;
   pragma export (C, Set_STKL_C, "set_stkl");
   
   procedure Set_VBEZ (VBEZ : in Cent_Type);
   function Set_VBEZ_C (VBEZ : in Cent_Type_C) return General_Type_C;
   pragma export (C, Set_VBEZ_C, "set_vbez");
   
   procedure Set_VBS (VBS : in Cent_Type);
   function Set_VBS_C (VBS : in Cent_Type_C) return General_Type_C;
   pragma export (C, Set_VBS_C, "set_vbs");
   
   procedure Set_VMT (VMT : in Cent_Type);
   function Set_VMT_C (VMT : in Cent_Type_C) return General_Type_C;
   pragma export (C, Set_VMT_C, "set_vmt");
   
   procedure Set_WFUNDF (WFUNDF : in Cent_Type);
   function Set_WFUNDF_C (WFUNDF : in Cent_Type_C) return General_Type_C;
   pragma export (C, Set_WFUNDF_C, "set_wfundf");
   
   procedure Set_ZKF (ZKF : in General_Type);
   function Set_ZKF_C (ZKF : in General_Type_C) return General_Type_C;
   pragma export (C, Set_ZKF_C, "set_zkf");
   
   procedure Set_ZVE (ZVE : in Cent_Type);
   function Set_ZVE_C (ZVE : in Cent_Type_C) return General_Type_C;
   pragma export (C, Set_ZVE_C, "set_zve");
   
   ------------------------
   -- Get PAP-parameters --
   ------------------------
   
   function Get_BK return Cent_Type;
   function Get_BK_C return Cent_Type_C;
   pragma export (C, Get_BK_C, "get_bk");
   
   function Get_BKS return Cent_Type;
   function Get_BKS_C return Cent_Type_C;
   pragma export (C, Get_BKS_C, "get_bks");
   
   function Get_BKV return Cent_Type;
   function Get_BKV_C return Cent_Type_C;
   pragma export (C, Get_BKV_C, "get_bkv");
   
   function Get_LSTLZZ return Cent_Type;
   function Get_LSTLZZ_C return Cent_Type_C;
   pragma export (C, Get_LSTLZZ_C, "get_lstlzz");
   
   function Get_LZALOG return Cent_Type;
   function Get_LZALOG_C return Cent_Type_C;
   pragma export (C, Get_LZALOG_C, "get_lzalog");
   
   function Get_LZALUG return Cent_Type;
   function Get_LZALUG_C return Cent_Type_C;
   pragma export (C, Get_LZALUG_C, "get_lzalug");
   
   function Get_SOLZLZZ return Cent_Type;
   function Get_SOLZLZZ_C return Cent_Type_C;
   pragma export (C, Get_SOLZLZZ_C, "get_solzlzz");
   
   function Get_SOLZS return Cent_Type;
   function Get_SOLZS_C return Cent_Type_C;
   pragma export (C, Get_SOLZS_C, "get_solzs");
   
   function Get_SOLZV return Cent_Type;
   function Get_SOLZV_C return Cent_Type_C;
   pragma export (C, Get_SOLZV_C, "get_solzv");
   
   function Get_STS return Cent_Type;
   function Get_STS_C return Cent_Type_C;
   pragma export (C, Get_STS_C, "get_sts");
   
   function Get_STV return Cent_Type;
   function Get_STV_C return Cent_Type_C;
   pragma export (C, Get_STV_C, "get_stv");
   
   -------------------------------
   -- Calculate Einkommensteuer --
   -------------------------------
   
   function Get_Grundtab return Cent_Type;
   function Get_Grundtab_C return Cent_Type_C;
   pragma export (C, Get_Grundtab_C, "get_grundtab");
   
   function Get_Splittab return Cent_Type;
   function Get_Splittab_C return Cent_Type_C;
   pragma export (C, Get_Splittab_C, "get_splittab");
   
   procedure Calc_Est;
   function Calc_Est_C return General_Type_C;
   pragma export (C, Calc_Est_C, "calc_est");
   
   --------------------------
   -- Calculate Lohnsteuer --
   --------------------------
   
   procedure Calc_Lst;
   function Calc_Lst_C return General_Type_C;
   pragma export (C, Calc_Lst_C, "calc_lst");
   
   ---------------------
   -- Reset-functions --
   ---------------------
   
   procedure Reset_All;
   function Reset_All_C return General_Type_C;
   pragma export (C, Reset_All_C, "reset_all");
   
   ----------
   -- Misc --
   ----------
   
   function Get_Min_Amount return Cent_Type;
   function Get_Min_Amount_C return Cent_Type_C;
   pragma export (C, Get_Min_Amount_C, "get_min_amount");
   
   function Get_Max_Amount return Cent_Type;
   function Get_Max_Amount_C return Cent_Type_C;
   pragma export (C, Get_Max_Amount_C, "get_max_amount");
   
private
   
   subtype Year_Type is Positive range 2002 .. 2004;
   Tax_Year : Year_Type := Year_Type'Last;
   
end LibOpenSteuer;
