with System;

package LibOpenSteuer.PAP is
   
   procedure Set_ALTER1 (ALTER1 : in General_Type);
   procedure Set_HINZUR (HINZUR : in Cent_Type);
   procedure Set_JFREIB (JFREIB : in Cent_Type);
   procedure Set_JHINZU (JHINZU : in Cent_Type);
   procedure Set_JRE4 (JRE4: in Cent_Type);
   procedure Set_JVBEZ (JVBEZ: in Cent_Type);
   procedure Set_KRV (KRV : in General_Type);
   procedure Set_KZTAB (KZTAB : in General_Type);
   procedure Set_LZZ (LZZ : in General_Type);
   procedure Set_R (R : in General_Type);
   procedure Set_RE4 (RE4: in Cent_Type);
   procedure Set_SONSTB (SONSTB: in Cent_Type);
   procedure Set_STKL (STKL : in General_Type);
   procedure Set_VBEZ (VBEZ : in Cent_Type);
   procedure Set_VBS (VBS : in Cent_Type);
   procedure Set_VMT (VMT : in Cent_Type);
   procedure Set_WFUNDF (WFUNDF : in Cent_Type);
   procedure Set_ZKF (ZKF : in General_Type);
   procedure Set_ZVE (ZVE : in Cent_Type);
   
   function Get_BK return Cent_Type;
   function Get_BKS return Cent_Type;
   function Get_BKV return Cent_Type;
   function Get_LSTLZZ return Cent_Type;
   function Get_LZALOG return Cent_Type;
   function Get_LZALUG return Cent_Type;
   function Get_SOLZLZZ return Cent_Type;
   function Get_SOLZS return Cent_Type;
   function Get_SOLZV return Cent_Type;
   function Get_ST return Cent_Type;
   function Get_STS return Cent_Type;
   function Get_STV return Cent_Type;
   
   procedure LstTab;
   procedure EstTab;
   
   function Get_Min_Amount return Cent_Type;
   function Get_Max_Amount return Cent_Type;
   
   procedure Reset_PAP_Intern;
   procedure Reset_PAP_Eingabe;
   
private
   
--   type Ten_Mio is range -10_000_000.0 .. 10_000_000.0;
   
end LibOpenSteuer.PAP;
