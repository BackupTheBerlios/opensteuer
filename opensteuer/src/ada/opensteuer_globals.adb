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

with Ada.Strings.Fixed;
-- with OpenSteuer_Debug; use OpenSteuer_Debug;

package body OpenSteuer_Globals is
   
   function Abrunden_0 (Zahl : RW_Intern) return Euro_0 is
   begin
      return Euro_0 (RW_Intern'Truncation (Zahl));
   end Abrunden_0;
   
   function Aufrunden_0 (Zahl : RW_Intern) return Euro_0 is
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
   
   function To_Integer (Zahl : RW_Intern) return Integer is
   begin
      return Integer (Zahl);
   end To_Integer;
   
   function To_Cent (Zahl : RW_Intern) return Cent_0 is
   begin
      return Cent_0 (Abrunden_2 (Zahl * 100.0));
   end To_Cent;
   
   function To_Euro (Zahl : RW_Intern) return Euro_0 is
   begin
      return Euro_0 (Zahl);
   end To_Euro;
   
   function To_RW_1 (Zahl : RW_Intern) return RW_1 is
   begin
      return RW_1 (Zahl);
   end To_RW_1;
   
   function To_RW_2 (Zahl : Cent_0) return RW_2 is
   begin
      return RW_2 (RW_Intern (Zahl) / 100.0);
   end To_RW_2;
   
   function Punkt_To_Komma (s : String) return String is
      t : String := s;
      Index_Pos : Natural := Ada.Strings.Fixed.Index (t, ".");
   begin
      if Index_Pos > 0 then
         Ada.Strings.Fixed.Overwrite (t, Index_Pos, ",");
      end if;
      return t;
   end Punkt_To_Komma;
   
   function Komma_To_Punkt (s : String) return String is
      t : String := s;
      Index_Pos : Natural := Ada.Strings.Fixed.Index (t, ",");
   begin
      if Index_Pos > 0 then
         Ada.Strings.Fixed.Overwrite (t, Index_Pos, ".");
      end if;
      return t;
   end Komma_To_Punkt;
   
end OpenSteuer_Globals;
