-- with OpenSteuer_Debug; use OpenSteuer_Debug;
-- with Ada.Text_IO; use Ada.Text_IO;
--with System;

package body LibOpenSteuer.PAP is

   function Abrunden_0 (Zahl : RW_Intern) return Euro is
   begin
      return Euro (RW_Intern'Truncation (Zahl));
   end Abrunden_0;
   
   function Aufrunden_0 (Zahl : RW_Intern) return Euro is
   begin
      return Abrunden_0 (Zahl) + 1;
   end Aufrunden_0;
   
   function Abrunden_1 (Zahl : RW_Intern) return RW_1 is
   begin
      return RW_1 (Zahl);
   end Abrunden_1;
   
   function Aufrunden_1 (Zahl : RW_Intern) return RW_1 is
   begin
      return Abrunden_1 (Zahl) + 0.1;
   end Aufrunden_1;
   
   function Abrunden_2 (Zahl : RW_Intern) return RW_2 is
   begin
      return RW_2 (Zahl);
   end Abrunden_2;
   
   function Aufrunden_2 (Zahl : RW_Intern) return RW_2 is
   begin
      return Abrunden_2 (Zahl) + 0.01;
   end Aufrunden_2;
   
   function Abrunden_3 (Zahl : RW_Intern) return RW_3 is
   begin
      return RW_3 (Zahl);
   end Abrunden_3;
   
   function Aufrunden_3 (Zahl : RW_Intern) return RW_3 is
   begin
      return Abrunden_3 (Zahl) + 0.001;
   end Aufrunden_3;
   
   -- Das ist eine 1 zu 1 Umsetzung des offiziellen PAP für das Steuerjahr 2002
   -- (siehe OpenSteuer-Homepage). Die Struktur mutet anachronistisch an. Ich
   -- habe ihn jedoch vorerst so umgesetzt, um dicht am Original zu bleiben.
   
   -- Seite 8
   
   procedure MRE4LZZ;
   procedure MRE4;
   procedure MZTABFB;
   procedure MLSTJAHR_1;
   procedure UPANTEIL;
   procedure MSOLZ;
   procedure MSONST;
   procedure MVMT;
   
   procedure LST2002 is
   begin
      MRE4LZZ;
      MRE4;
      MZTABFB;
      MLSTJAHR_1;
      LSTJAHR := ST;
      JW := Cent (LSTJAHR * 100);
      UPANTEIL;
      LSTLZZ := ANTEIL1;
      if ZKF > 0.0 then
         ZTABFB := ZTABFB + KFB;
         MLSTJAHR_1;
         JBMG := ST;
      else
         JBMG := LSTJAHR;
      end if;
      MSOLZ;
      MSONST;
      MVMT;
   end LST2002;
   
   -- Seite 9
   
   procedure MRE4LZZ is
   begin
      if VBEZ = 0 then
         FVB := 0;
      else
         FVB := Cent (Aufrunden_0 (RW_Intern (VBEZ) * 0.4)); -- auf volle Cent aufrunden
         JW := 307200;
         UPANTEIL;
         if FVB > ANTEIL2 then
            FVB := ANTEIL2;
         end if;
      end if;
      if ALTER1 = 0 then
         ALTE := 0;
      else
         BMG := RE4 - VBEZ;
         ALTE := Cent (Aufrunden_0 (RW_Intern (BMG) * 0.4)); -- auf volle Cent aufrunden
         JW := 190800;
         UPANTEIL;
         if ALTE > ANTEIL2 then
            ALTE := ANTEIL2;
         end if;
      end if;
      RE4LZZ := RE4 - FVB - ALTE - WFUNDF + HINZUR;
      RE4LZZV := RE4 - FVB - ALTE;
   end MRE4LZZ;
   
   -- Seite 10
   
   procedure UPRUND36;
   
   procedure MRE4 is
   begin
      case LZZ is
         when 1 =>
            ZRE4 := RW_2 (RW_Intern (RE4LZZ) / 100.0);
            ZRE4VP := RW_2 (RW_Intern (RE4LZZV) / 100.0);
         when 2 =>
            ZRE4 := Abrunden_2 ((RW_Intern (RE4LZZ) + 0.67) * 0.12); -- auf volle Cent abrunden + *) Seite 10
            ZRE4VP := Abrunden_2 ((RW_Intern (RE4LZZV) + 0.67) * 0.12); -- auf volle Cent abrunden + *) Seite 10
         when 3 =>
            ZRE4 := Abrunden_2 ((RW_Intern (RE4LZZ) + 0.89) * 3.6 / 7.0); -- auf volle Cent abrunden + *) Seite 10
            ZRE4VP := Abrunden_2 ((RW_Intern (RE4LZZV) + 0.89) * 3.6 / 7.0); -- auf volle Cent abrunden + *) Seite 10
         when others =>
            ZRE4 := Abrunden_2 ((RW_Intern (RE4LZZ) + 0.56) * 3.6); -- auf volle Cent abrunden + *) Seite 10
            ZRE4VP := Abrunden_2 ((RW_Intern (RE4LZZV) + 0.56) * 3.6); -- auf volle Cent abrunden + *) Seite 10
      end case;
      if ZRE4 < 0.0 then
         ZRE4 := 0.0;
      end if;
      RUND := Abrunden_0 (RW_Intern (ZRE4)); -- testen, was hier bei rauskommt!
      UPRUND36;
      RE4U := RUND;
      RUND := Abrunden_0 (RW_Intern (ZRE4VP)); -- wie oben!
      UPRUND36;
      RE4O := RUND + 35;
      JW := Cent (RE4U * 100);
      UPANTEIL;
      LZALUG := ANTEIL1;
      case LZZ is
         when 1 => LZALOG := LZALUG + 3599;
         when 2 => LZALOG := LZALUG + 299;
         when 3 => LZALOG := LZALUG + 69;
         when 4 => LZALOG := LZALUG + 9;
      end case;
   end MRE4;
   
   -- Seite 11
   
   procedure MZTABFB is
   begin
      KZTAB := 1;
      case STKL is
         when 1 =>
            ANP := 1044;
            SAP := 36;
            KFB := Euro (RW_Intern (ZKF) * 5808.0); -- Test, ob keine Nachkommastellen verloren gehen!
         when 2 =>
            HFB := 2340;
            ANP := 1044;
            SAP := 36;
            KFB := Euro (RW_Intern (ZKF) * 5808.0); -- wie oben!
         when 3 =>
            KZTAB := 2;
            ANP := 1044;
            SAP := 72;
            KFB := Euro (RW_Intern (ZKF) * 5808.0); -- wie oben!
         when 4 =>
            ANP := 1044;
            SAP := 36;
            KFB := Euro (RW_Intern (ZKF) * 2904.0); -- wie oben!
         when 5 =>
            ANP := 1044;
            KFB := 0;
         when 6 =>
            KFB := 0;
      end case;
      ZTABFB := HFB + ANP + SAP;
   end MZTABFB;
   
   -- Seite 12
   
   procedure MVSP;
   -- procedure UPTAB02;
   procedure MST5_6;
   -- procedure MLSTJAHR_2;
   
   procedure MLSTJAHR_1 is
   begin
      if STKL < 5 then
         MVSP;
      else
         VSP := 0.0;
      end if;
      ZVE := RE4U - ZTABFB - Euro (RW_Intern (VSP)); -- Test, ob VSP keine Nachkommastellen hat!
      MLSTJAHR_2;
   end MLSTJAHR_1;
   
   procedure MLSTJAHR_2 is
   begin
      if ZVE < 36 then
         ZVE := 0;
         X := 0.0;
      else
         RUND := ZVE / Euro (KZTAB);
         UPRUND36;
         X := RW_1 (RUND + 18);
      end if;
      if STKL < 5 then
         UPTAB02;
      else
         MST5_6;
      end if;
   end MLSTJAHR_2;
   
   -- Seite 13
   
   procedure UMVSP;
   
   procedure MVSP is
   begin
      VSPO := RW_2 (RW_Intern (RE4O) * 0.20); -- prüfen, sollte aber ok sein
      VSPVOR := RW_2 (RW_Intern (3068 * RW_2 (KZTAB))); -- nächste vier Zeilen prüfen!
      VSPMAX1 := Euro (RW_Intern (1334 * Euro (KZTAB)));
      VSPMAX2 := Euro (RW_Intern (667 * Euro (KZTAB)));
      VSPKURZ := Euro (RW_Intern (1134 * Euro (KZTAB)));
      if KRV = 1 then
         if VSPO > RW_2 (VSPKURZ) then
            VSP := RW_2 (VSPKURZ);
         else
            VSP := VSPO;
         end if;
      else
         UMVSP;
      end if;
      RUND := Abrunden_0 (RW_Intern (VSP));  -- hier testen, ob VSP keine Nachkommastellen hat
      UPRUND36;
      VSP := RW_2 (RUND);
   end MVSP;
   
   -- Seite 14
   
   procedure UP5_6 is
   begin
      X := RW_1 (RW_Intern (ZX) * 1.25); -- prüfen, ob 2. Nachkommastelle auftritt. Dürfte nicht sein.
      UPTAB02;
      ST1 := ST;
      X := RW_1 (RW_Intern (ZX) * 0.75); -- wie oben.
      UPTAB02;
      ST2 := ST;
      DIFF := (ST1 - ST2) * 2;
      MIST := Abrunden_0 (RW_Intern (ZX) * 0.199); -- auf volle EUR abrunden.
      if MIST > DIFF then
         ST := MIST;
      else
         ST := DIFF;
      end if;
   end UP5_6;
   
   procedure MST5_6 is
   begin
      ZZX := Euro (Abrunden_0 (RW_Intern (X))); -- prüfen, ob X tatsächlich keine Nachkommastelle hier hat!
      if ZZX > 27306 then
         ZX := 27306;
         UP5_6;
         ST := Abrunden_0 (RW_Intern (ST) + (RW_Intern (ZZX - 27306)) * 0.485); -- auf volle EUR abrunden.
      else
         ZX := ZZX;
         UP5_6;
         if ZZX > 8946 then
            VERGL := ST;
            ZX := 8946;
            UP5_6;
            HOCH := Abrunden_0 (RW_Intern (ST) + (RW_Intern (ZZX - 8946)) * 0.485); -- auf volle EUR abrunden.
            if HOCH < VERGL then
               ST := HOCH;
            else
               ST := VERGL;
            end if;
         end if;
      end if;
   end MST5_6;
   
   -- Seite 15
   
   procedure MSOLZ is
   begin
      SOLZFREI := 972 * Euro (KZTAB);
      if JBMG > SOLZFREI then
         SOLZJ := Abrunden_2 (RW_Intern (JBMG) * 5.5 / 100.0); -- auf volle Cent abrunden.
         SOLZMIN := RW_2 (RW_Intern (JBMG - SOLZFREI) * 20.0 / 100.0);
         if SOLZMIN < SOLZJ then
            SOLZJ := SOLZMIN;
         end if;
         JW := Cent (SOLZJ * 100);
         UPANTEIL;
         SOLZLZZ := ANTEIL1;
      else
         SOLZLZZ := 0;
      end if;
      if R > 0 then
         JW := Cent (JBMG * 100);
         UPANTEIL;
         BK := ANTEIL1;
      else
         BK := 0;
      end if;
   end MSOLZ;
   
   -- Seite 16
   
   procedure UMVSP is
   begin
      VSPVOR := RW_2 (RW_Intern (VSPVOR) - RW_Intern (RE4O) * 0.16);
      if VSPVOR < 0.0 then
         VSPVOR := 0.0;
      end if;
      if VSPO > VSPVOR then
         VSP := VSPVOR;
         VSPREST := VSPO - VSPVOR;
         if VSPREST > RW_2 (VSPMAX1) then
            VSP := VSP + RW_2 (VSPMAX1);
            VSPREST := Aufrunden_2 ((RW_Intern (VSPREST) - RW_Intern (VSPMAX1)) / 2.0); -- auf volle Cent aufrunden.
            if VSPREST > RW_2 (VSPMAX2) then
               VSP := VSP + RW_2 (VSPMAX2);
            else
               VSP := VSP + VSPREST;
            end if;
         else
            VSP := VSP + VSPREST;
         end if;
      else
         VSP := VSPO;
      end if;
   end UMVSP;
   
   -- Seite 17
   
   procedure UPRUND36 is
   begin
      RUND := RUND / 36;
      RUND := RUND * 36;
   end UPRUND36;
   
   procedure UPANTEIL is
   begin
      case LZZ is
         when 1 =>
            ANTEIL1 := JW;
            ANTEIL2 := JW;
         when 2 =>
            ANTEIL1 := Cent (Abrunden_0 (RW_Intern (JW) / 12.0)); -- Ergebnis abrunden.
            ANTEIL2 := Cent (Aufrunden_0 (RW_Intern (JW) / 12.0)); -- Ergebnis aufrunden.
         when 3 =>
            ANTEIL1 := Cent (Abrunden_0 (RW_Intern (JW) * 7.0 / 360.0)); -- Ergebnis abrunden.
            ANTEIL2 := Cent (Aufrunden_0 (RW_Intern (JW) * 7.0 / 360.0)); -- Ergebnis aufrunden.
         when others =>
            ANTEIL1 := Cent (Abrunden_0 (RW_Intern (JW) / 360.0)); -- Ergebnis abrunden.
            ANTEIL2 := Cent (Aufrunden_0 (RW_Intern (JW) / 360.0)); -- Ergebnis aufrunden.
      end case;
   end UPANTEIL;
   
   -- Seite 18
   
   procedure MLSTBER;
   
   procedure MSONST is
   begin
      if SONSTB = 0 then -- Vorsicht Test auf 0!
         STS := 0;
         SOLZS := 0;
         BKS := 0;
      else
         if SONSTB > 15000 then
            LZZ := 1;
            VBEZ := JVBEZ;
            WFUNDF := JFREIB;
            HINZUR := JHINZU;
            RE4 := JRE4;
            MLSTBER;
            LST1 := Cent (LSTJAHR * 100);
            VBEZ := JVBEZ + VBS;
            RE4 := JRE4 + SONSTB;
            MLSTBER;
            LST2 := Cent (LSTJAHR * 100);
            STS := LST2 - LST1;
         else
            VBEZ := VBEZ + VBS;
            RE4 := RE4 + SONSTB;
            MLSTBER;
            JW := Cent (LSTJAHR * 100);
            UPANTEIL;
            LSTLZZS := ANTEIL1;
            STS := LSTLZZS - LSTLZZ;
         end if;
         SOLZS := Cent (Abrunden_0 (RW_Intern (STS) * 5.5 / 100.0)); -- auf volle Cent abrunden.
         if R > 0 then
            BKS := STS;
         else
            BKS := 0;
         end if;
      end if;
   end MSONST;
   
   procedure MLSTBER is
   begin
      MRE4LZZ;
      MRE4;
      MZTABFB;
      MLSTJAHR_1;
      LSTJAHR := ST;
   end MLSTBER;
   
   -- Seite 19
   
   procedure MVMT is
   begin
      if VMT = 0 then -- Vorsicht, Test auf 0!
         STV := 0;
         SOLZV := 0;
         BKV := 0;
      else
         LZZ := 1;
         VBEZ := JVBEZ + VBS;
         RE4 := JRE4 + SONSTB;
         WFUNDF := JFREIB;
         HINZUR := JHINZU;
         MLSTBER;
         LST1 := Cent (LSTJAHR * 100);
         RE4 := JRE4 + SONSTB + VMT / 5;
         MLSTBER;
         LST2 := Cent (LSTJAHR * 100);
         STV := (LST2 - LST1) * 5;
         RE4 := JRE4 + SONSTB + VMT;
         MLSTBER;
         LST3 := Cent (LSTJAHR * 100);
         LST3 := LST3 - LST1;
         if LST3 < STV then
            STV := LST3;
         end if;
         SOLZV := Cent (Abrunden_0 (RW_Intern (STV) * 5.5 / 100.0)); -- auf volle Cent abrunden;
         if R > 0  then
            BKV := STV;
         else
            BKV := 0;
         end if;
      end if;
   end MVMT;
   
   -- Seite 20
   
   procedure UPTAB02 is
   begin
      if X < 7236.0 then
         ST := 0;
      elsif X < 9252.0 then
         Y := (X - 7200.0) / 10000.0;
         RW := Abrunden_3 (RW_Intern (Y) * 768.85); -- Dezimalstellen ab 4. Stelle weglassen.
         RW := RW + 1990.0;
         ST := Abrunden_0 (RW_Intern (RW) * RW_Intern (Y)); -- auf volle EUR abrunden.
      elsif X < 55008.0 then
         Y := (X - 9216.0) / 10000.0;
         RW := Abrunden_3 (RW_Intern (Y) * 278.65); -- Dezimalstellen ab 4. Stelle weglassen.
         RW := RW + 2300.0;
         RW := Abrunden_3 (RW_Intern (RW) * RW_Intern (Y)); -- Dezimalstellen ab 4. Stelle weglassen.
         ST := Abrunden_0 (RW_Intern (RW) + 432.0); -- auf volle EUR abrunden.
      else
         ST := Abrunden_0 (RW_Intern (X) * 0.485 - RW_Intern (9872)); -- auf volle EUR abrunden.
      end if;
      ST := ST * Euro (KZTAB);
   end UPTAB02;
   
   procedure Reset_PAP_Eingabe is
   begin
      ALTER1 := 0;
      HINZUR := 0; -- in Cent
      JFREIB := 0; -- in Cent
      JHINZU := 0; -- in Cent
      JRE4 := 0; -- in Cent
      JVBEZ := 0; -- in Cent
      KRV := 0;
      LZZ := 2;
      R := 0;
      RE4 := 0; -- in Cent
      SONSTB := 0; -- in Cent
      STKL := 1;
      VBEZ := 0; -- in Cent
      VBS := 0; -- in Cent
      VMT := 0; -- in Cent
      WFUNDF := 0; -- in Cent
      ZKF := 0.0;
   end Reset_PAP_Eingabe;
   
   procedure Reset_PAP_Intern is
   begin
      ALTE := 0;
      ANP := 0;
      ANTEIL1 := 0;
      ANTEIL2 := 0;
      BMG := 0;
      DIFF := 0;
      FVB := 0;
      HFB := 0;
      JBMG := 0;
      JW := 0;
      KFB := 0;
      KZTAB := 1;
      LSTJAHR := 0;
      LSTLZZS := 0;
      LST1 := 0;
      LST2 := 0;
      LST3 := 0;
      MIST := 0;
      RE4LZZ := 0;
      RE4LZZV := 0;
      RE4O := 0;
      RE4U := 0;
      RUND := 0;
      RW := 0.0;
      SAP := 0;
      SOLZFREI := 0;
      SOLZJ := 0.0;
      SOLZMIN := 0.0;
      ST := 0;
      ST1 := 0;
      ST2 := 0;
      TW := 0;
      VSP := 0.0;
      VSPKURZ := 0;
      VSPMAX1 := 0;
      VSPMAX2 := 0;
      VSPO := 0.0;
      VSPREST := 0.0;
      VSPVOR := 0.0;
      X := 0.0;
      Y := 0.0;
      ZRE4 := 0.0;
      ZRE4VP := 0.0;
      ZTABFB := 0;
      ZVE := 0;
      ZX := 0;
      ZZX := 0;
      HOCH := 0;
      VERGL := 0;
   end Reset_PAP_Intern;
   
   begin
      Reset_PAP_Eingabe;
      Reset_PAP_Intern;
      
end LibOpenSteuer.PAP;
