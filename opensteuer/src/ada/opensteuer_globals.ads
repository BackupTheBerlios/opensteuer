------------------------------------------------------------------------------
-- OpenSteuer -  An open-source taxprogram based on german tax laws         --
--                                                                          --
-- The latest version is available at:                                      --
--    http://www.opensteuer.de                                              --
------------------------------------------------------------------------------
-- COPYRIGHT (C) 2003:                                                      --
--    Hannes Birnbacher, Martin Klaiber, Sigrid Wörsdörfer.                 --
--                                                                          --
-- AUTHOR:                                                                  --
--    Martin Klaiber.                                                       --
--                                                                          --
-- LICENCE:                                                                 --
--    This file is part of OpenSteuer.                                      --
--                                                                          --
--    OpenSteuer is free software; you can redistribute it and/or modify    --
--    it under the terms of the GNU General Public License as published by  --
--    the Free Software Foundation; either version 2 of the License, or     --
--    (at your option) any later version.                                   --
--                                                                          --
--    OpenSteuer is distributed in the hope that it will be useful, but     --
--    WITHOUT ANY WARRANTY; without even the implied warranty of            --
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     --
--    General Public License for more details.                              --
--                                                                          --
--    You should have received a copy of the GNU General Public License     --
--    along with OpenSteuer;  see file COPYING.  If not, write to the       --
--       Free Software Foundation, Inc.                                     --
--       59 Temple Place, Suite 330                                         --
--       Boston, MA 02111-1307                                              --
--       USA                                                                --
------------------------------------------------------------------------------

with System;

package OpenSteuer_Globals is
   
   --------------------------------------
   -- Die Zahlentypen für Berechnungen --
   --------------------------------------
   
   -- Maximale Float-Genauigkeit ist:
   OpenSteuer_Max_Digits : constant := System.Max_Base_Digits;
   
   -- der PAP verlangt insgesamt sechs Typen:
   
   -- 1) Betrag in EUR ohne Nachkommastellen:
   type Euro_0 is range System.Min_Int..System.Max_Int;
   
   -- 2) Betrag in Cent ohne Nachkommastellen:
   type Cent_0 is range System.Min_Int..System.Max_Int;
   
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
   
   OpenSteuer_Version : String := "0.1.0";
   
   OpenSteuer_Border_Width : Natural := 10;
   
   Steuer_Jahr : Positive := 2002;
   
   ---------------------------------------
   -- Globale Prozeduren und Funktionen --
   ---------------------------------------
   
   function Abrunden_0 (Zahl : RW_Intern) return Euro_0;
   function Aufrunden_0 (Zahl : RW_Intern) return Euro_0;
   function Abrunden_1 (Zahl : RW_Intern) return RW_1;
   function Aufrunden_1 (Zahl : RW_Intern) return RW_1;
   function Abrunden_2 (Zahl : RW_Intern) return RW_2;
   function Aufrunden_2 (Zahl : RW_Intern) return RW_2;
   function Abrunden_3 (Zahl : RW_Intern) return RW_3;
   function Aufrunden_3 (Zahl : RW_Intern) return RW_3;
   
   function To_Integer (Zahl : RW_Intern) return Integer;
   function To_Cent (Zahl : RW_Intern) return Cent_0;
   function To_Euro (Zahl : RW_Intern) return Euro_0;
   function To_RW_1 (Zahl : RW_Intern) return RW_1;
   function To_RW_2 (Zahl : Cent_0) return RW_2;
   
   function Punkt_To_Komma (s : String) return String;
   function Komma_To_Punkt (s : String) return String;
   
end OpenSteuer_Globals;
