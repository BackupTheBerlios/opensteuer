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

with Gtk.Window; use Gtk.Window;
with Gtk.Button; use Gtk.Button;
with Gtk.Spin_Button; use Gtk.Spin_Button;
with Gtk.Radio_Button; use Gtk.Radio_Button;
with Gtk.Label; use Gtk.Label;
with OpenSteuer_Entry; use OpenSteuer_Entry;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Separator; use Gtk.Separator;
with Gtk.Notebook; use Gtk.Notebook;
with Gtk.Box; use Gtk.Box;
with OpenSteuer_Globals; use OpenSteuer_Globals;

package OpenSteuer_LstTab is
   
   procedure Lohnsteuer;
   
private
   
   type Stkl_Label_Array is array (STKL_Type'Range) of Gtk_Label;
   type Notebook_Pages is new Positive Range 1..2;
   type Spin_Entry_Array is array (Notebook_Pages'Range) of Spin_Entry_Access;
   type Num_Entry_Array is array (Notebook_Pages'Range) of Num_Entry_Access;
   type Radio_Button_Array is array (Notebook_Pages'Range) of Gtk_Radio_Button;
   
   type Lohnsteuer_Record is new Gtk_Window_Record with
      record
         Win : Gtk_Window;
         VBox : Gtk_Box;
         Ausgabe_Oben : Boolean := False;
         Eingabe : Gtk_Notebook;
         Ausgabe : Gtk_Scrolled_Window;
         Separator1, Separator2 : Gtk_Separator;
         ALTER1_Spin_Entry,
         ZKF_Spin_Entry : Spin_Entry_Array;
         HINZUR_Entry : Num_Entry_Access;
         JFREIB_Entry : Num_Entry_Access;
         JHINZU_Entry : Num_Entry_Access;
         JRE4_Entry : Num_Entry_Access;
         JVBEZ_Entry : Num_Entry_Access;
         KRV_1_Ja, KRV_0_Nein : Gtk_Radio_Button;
         SONSTB_Entry : Num_Entry_Access;
         VBEZ_Entry : Num_Entry_Access;
         VBS_Entry : Num_Entry_Access;
         VMT_Entry : Num_Entry_Access;
         WFUNDF_Entry : Num_Entry_Access;
         LZZ_Jahr, LZZ_Monat, LZZ_Woche, LZZ_Tag : Radio_Button_Array;
         RE4_Entry : Num_Entry_Array;
         -- Die Ausgabe:
         LSTLZZ_Array, SOLZLZZ_Array : Stkl_Label_Array;
         -- Für die Kirchensteuer:
         K8_Array, K9_Array : Stkl_Label_Array;
      end record;
   
   type Lohnsteuer_Access is access all Lohnsteuer_Record'Class;
   
   procedure Gtk_New (Lohnsteuer : out Lohnsteuer_Access);
   procedure Initialize (Lohnsteuer : Lohnsteuer_Access);
   procedure Handle_Calculate (
      Button : access Gtk_Button_Record'Class;
      Lohnsteuer : Lohnsteuer_Access);
   procedure Handle_Page_Switch (
      Notebook : access Gtk_Notebook_Record'Class;
      Lohnsteuer : Lohnsteuer_Access);
   procedure Update_Variables (Lohnsteuer : Lohnsteuer_Access);
   procedure Update_Entries (Lohnsteuer : Lohnsteuer_Access);
   procedure Calculate (Lohnsteuer : Lohnsteuer_Access);
   procedure Einstellungen (
      Button : access Gtk_Button_Record'Class;
      Lohnsteuer : Lohnsteuer_Access);
      
end OpenSteuer_LstTab;
