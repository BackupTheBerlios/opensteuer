with LibOpenSteuer.PAP.Y2002;
with LibOpenSteuer.PAP.Y2004;

package body LibOpenSteuer.PAP is
   
   procedure Set_ALTER1 (ALTER1 : in General_Type) is
   begin
      case Tax_Year is
         when 2002 | 2003 => Y2002.Set_ALTER1 (ALTER1);
         when 2004 => Y2004.Set_ALTER1 (ALTER1);
      end case;
   end Set_ALTER1;
   
   procedure Set_HINZUR (HINZUR : Cent_Type) is
   begin
      case Tax_Year is
         when 2002 | 2003 => Y2002.Set_HINZUR (HINZUR);
         when 2004 => Y2004.Set_HINZUR (HINZUR);
      end case;
   end Set_HINZUR;
   
   procedure Set_JFREIB (JFREIB : Cent_Type) is
   begin
      case Tax_Year is
         when 2002 | 2003 => Y2002.Set_JFREIB (JFREIB);
         when 2004 => Y2004.Set_JFREIB (JFREIB);
      end case;
   end Set_JFREIB;
   
   procedure Set_JHINZU (JHINZU : Cent_Type) is
   begin
      case Tax_Year is
         when 2002 | 2003 => Y2002.Set_JHINZU (JHINZU);
         when 2004 => Y2004.Set_JHINZU (JHINZU);
      end case;
   end Set_JHINZU;
   
   procedure Set_JRE4 (JRE4: Cent_Type) is
   begin
      case Tax_Year is
         when 2002 | 2003 => Y2002.Set_JRE4 (JRE4);
         when 2004 => Y2004.Set_JRE4 (JRE4);
      end case;
   end Set_JRE4;
   
   procedure Set_JVBEZ (JVBEZ: Cent_Type) is
   begin
      case Tax_Year is
         when 2002 | 2003 => Y2002.Set_JVBEZ (JVBEZ);
         when 2004 => Y2004.Set_JVBEZ (JVBEZ);
      end case;
   end Set_JVBEZ;
   
   procedure Set_KRV (KRV : in General_Type) is
   begin
      case Tax_Year is
         when 2002 | 2003 => Y2002.Set_KRV (KRV);
         when 2004 => Y2004.Set_KRV (KRV);
      end case;
   end Set_KRV;
   
   procedure Set_KZTAB (KZTAB : in General_Type) is
   begin
      case Tax_Year is
         when 2002 | 2003 => Y2002.Set_KZTAB (KZTAB);
         when 2004 => Y2004.Set_KZTAB (KZTAB);
      end case;
   end Set_KZTAB;
   
   procedure Set_LZZ (LZZ : in General_Type) is
   begin
      case Tax_Year is
         when 2002 | 2003 => Y2002.Set_LZZ (LZZ);
         when 2004 => Y2004.Set_LZZ (LZZ);
      end case;
   end Set_LZZ;
   
   procedure Set_RE4 (RE4: Cent_Type) is
   begin
      case Tax_Year is
         when 2002 | 2003 => Y2002.Set_RE4 (RE4);
         when 2004 => Y2004.Set_RE4 (RE4);
      end case;
   end Set_RE4;
   
   procedure Set_R (R : in General_Type) is
   begin
      case Tax_Year is
         when 2002 | 2003 => Y2002.Set_R (R);
         when 2004 => Y2004.Set_R (R);
      end case;
   end Set_R;
   
   procedure Set_SONSTB (SONSTB: Cent_Type) is
   begin
      case Tax_Year is
         when 2002 | 2003 => Y2002.Set_SONSTB (SONSTB);
         when 2004 => Y2004.Set_SONSTB (SONSTB);
      end case;
   end Set_SONSTB;
   
   procedure Set_STKL (STKL : in General_Type) is
   begin
      case Tax_Year is
         when 2002 | 2003 => Y2002.Set_STKL (STKL);
         when 2004 => Y2004.Set_STKL (STKL);
      end case;
   end Set_STKL;
   
   procedure Set_VBEZ (VBEZ : Cent_Type) is
   begin
      case Tax_Year is
         when 2002 | 2003 => Y2002.Set_VBEZ (VBEZ);
         when 2004 => Y2004.Set_VBEZ (VBEZ);
      end case;
   end Set_VBEZ;
   
   procedure Set_VBS (VBS : Cent_Type) is
   begin
      case Tax_Year is
         when 2002 | 2003 => Y2002.Set_VBS (VBS);
         when 2004 => Y2004.Set_VBS (VBS);
      end case;
   end Set_VBS;
   
   procedure Set_VMT (VMT : Cent_Type) is
   begin
      case Tax_Year is
         when 2002 | 2003 => Y2002.Set_VMT (VMT);
         when 2004 => Y2004.Set_VMT (VMT);
      end case;
   end Set_VMT;
   
   procedure Set_WFUNDF (WFUNDF : Cent_Type) is
   begin
      case Tax_Year is
         when 2002 | 2003 => Y2002.Set_WFUNDF (WFUNDF);
         when 2004 => Y2004.Set_WFUNDF (WFUNDF);
      end case;
   end Set_WFUNDF;
   
   procedure Set_ZKF (ZKF : in General_Type) is
   begin
      case Tax_Year is
         when 2002 | 2003 => Y2002.Set_ZKF (ZKF);
         when 2004 => Y2004.Set_ZKF (ZKF);
      end case;
   end Set_ZKF;
   
   procedure Set_ZVE (ZVE : in Cent_Type) is
   begin
      case Tax_Year is
         when 2002 | 2003 => Y2002.Set_ZVE (ZVE);
         when 2004 => Y2004.Set_ZVE (ZVE);
      end case;
   end Set_ZVE;
   
   ---------
   -- Get --
   ---------
   
   function Get_BK return Cent_Type is
   begin
      case Tax_Year is
         when 2002 | 2003 => return Y2002.Get_BK;
         when 2004 => return Y2004.Get_BK;
      end case;
   end Get_BK;
   
   function Get_BKS return Cent_Type is
   begin
      case Tax_Year is
         when 2002 | 2003 => return Y2002.Get_BKS;
         when 2004 => return Y2004.Get_BKS;
      end case;
   end Get_BKS;
   
   function Get_BKV return Cent_Type is
   begin
      case Tax_Year is
         when 2002 | 2003 => return Y2002.Get_BKV;
         when 2004 => return Y2004.Get_BKV;
      end case;
   end Get_BKV;
   
   function Get_LSTLZZ return Cent_Type is
   begin
      case Tax_Year is
         when 2002 | 2003 => return Y2002.Get_LSTLZZ;
         when 2004 => return Y2004.Get_LSTLZZ;
      end case;
   end Get_LSTLZZ;
   
   function Get_LZALOG return Cent_Type is
   begin
      case Tax_Year is
         when 2002 | 2003 => return Y2002.Get_LZALOG;
         when 2004 => raise Parameter_Not_Defined_Error;
      end case;
   end Get_LZALOG;
   
   function Get_LZALUG return Cent_Type is
   begin
      case Tax_Year is
         when 2002 | 2003 => return Y2002.Get_LZALUG;
         when 2004 => raise Parameter_Not_Defined_Error;
      end case;
   end Get_LZALUG;
   
   function Get_SOLZLZZ return Cent_Type is
   begin
      case Tax_Year is
         when 2002 | 2003 => return Y2002.Get_SOLZLZZ;
         when 2004 => return Y2004.Get_SOLZLZZ;
      end case;
   end Get_SOLZLZZ;
   
   function Get_SOLZS return Cent_Type is
   begin
      case Tax_Year is
         when 2002 | 2003 => return Y2002.Get_SOLZS;
         when 2004 => return Y2004.Get_SOLZS;
      end case;
   end Get_SOLZS;
   
   function Get_SOLZV return Cent_Type is
   begin
      case Tax_Year is
         when 2002 | 2003 => return Y2002.Get_SOLZV;
         when 2004 => return Y2004.Get_SOLZV;
      end case;
   end Get_SOLZV;
   
   function Get_STS return Cent_Type is
   begin
      case Tax_Year is
         when 2002 | 2003 => return Y2002.Get_STS;
         when 2004 => return Y2004.Get_STS;
      end case;
   end Get_STS;
   
   function Get_ST return Cent_Type is
   begin
      case Tax_Year is
         when 2002 | 2003 => return Y2002.Get_ST;
         when 2004 => return Y2004.Get_ST;
      end case;
   end Get_ST;
   
   function Get_STV return Cent_Type is
   begin
      case Tax_Year is
         when 2002 | 2003 => return Y2002.Get_STV;
         when 2004 => return Y2004.Get_STV;
      end case;
   end Get_STV;
   
   ----------
   -- Calc --
   ----------
   
   procedure LstTab is
   begin
      case Tax_Year is
         when 2002 | 2003 => Y2002.LST2002;
         when 2004 => Y2004.LST2004;
      end case;
   end LstTab;
   
   procedure EstTab is
   begin
      case Tax_Year is
         when 2002 | 2003 =>
            Y2002.MLSTJAHR_2;
            Y2002.UPTAB02;
         when 2004 =>
            Y2004.MLSTJAHR_2;
            Y2004.UPTAB04;
      end case;
   end EstTab;
   
   ---------
   -- Rechenbereich --
   -------------------
   
   function Get_Min_Amount return Cent_Type is
   begin
      case Tax_Year is
         when 2002 | 2003 => return Y2002.Get_Min_Amount;
         when 2004 => return Y2004.Get_Min_Amount;
      end case;
   end Get_Min_Amount;
   
   function Get_Max_Amount return Cent_Type is
   begin
      case Tax_Year is
         when 2002 | 2003 => return Y2002.Get_Max_Amount;
         when 2004 => return Y2004.Get_Max_Amount;
      end case;
   end Get_Max_Amount;
   
   -----------
   -- Reset --
   -----------
   
   procedure Reset_PAP_Intern is
   begin
      case Tax_Year is
         when 2002 | 2003 => Y2002.Reset_PAP_Intern;
         when 2004 => Y2004.Reset_PAP_Intern;
      end case;
   end Reset_PAP_Intern;
   
   procedure Reset_PAP_Eingabe is
   begin
      case Tax_Year is
         when 2002 | 2003 => Y2002.Reset_PAP_Eingabe;
         when 2004 => Y2004.Reset_PAP_Eingabe;
      end case;
   end Reset_PAP_Eingabe;
   
end LibOpenSteuer.PAP;
