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

with Ada.Strings.Fixed; use Ada.Strings.Fixed;
-- with Ada.Text_IO; use Ada.Text_IO;
with Gtk; use Gtk;
with Gtk.Window; use Gtk.Window;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Label; use Gtk.Label;
with Gdk.Event; use Gdk.Event;
with Gtk.Table; use Gtk.Table;
with Gtk.Separator; use Gtk.Separator;
with Gtk.Adjustment; use Gtk.Adjustment;
with Glib; use Glib;
with Gtk.Box; use Gtk.Box;
with Gtk.Button; use Gtk.Button;
with Gtk.Notebook; use Gtk.Notebook;
with Gtk.GEntry; use Gtk.GEntry;
with OpenSteuer_PAP; use OpenSteuer_PAP;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with OpenSteuer_Globals; use OpenSteuer_Globals;
with Gtk.Handlers;
with Gtk.Marshallers;
with OpenSteuer_Entry; use OpenSteuer_Entry;
-- with OpenSteuer_Debug; use OpenSteuer_Debug;

package body OpenSteuer_LstTab is
   
   ----------------
   -- Lohnsteuer --
   ----------------
   
   package Handlers is new Gtk.Handlers.Callback (Widget_Type => Gtk_Widget_Record);
   package My_Button_Handler is new Gtk.Handlers.User_Callback (Gtk_Button_Record, Lohnsteuer_Access);
   package My_Switch_Page_Handler is new Gtk.Handlers.User_Callback (Gtk_Notebook_Record, Lohnsteuer_Access);
   
   procedure Gtk_New (Lohnsteuer : out Lohnsteuer_Access) is
   begin
      Lohnsteuer := new Lohnsteuer_Record;
      Initialize (Lohnsteuer);
   end Gtk_New;
   
   -- Der Durchschnittsuser sei 30 Jahre alt:
   Geburtsjahr : Gfloat := 1973.0;
   
   procedure Initialize (Lohnsteuer : Lohnsteuer_Access) is
      
      procedure Make_Eingabe is
         Table : Gtk_Table;
         Adjustment : Gtk_Adjustment;
         Kuerzel, Description : Gtk_Label;
         
         procedure Make_Kuerzel (K : String; l : Guint) is
         begin
            Gtk_New (Kuerzel, K);
            Attach (Table, Kuerzel, 0, 1, l - 1, l,
            XPadding => Guint (OpenSteuer_Border_Width));
            Set_Alignment (Kuerzel, 0.0, 0.5);
         end Make_Kuerzel;
         
         procedure Make_Description (D : String; l, v, b : Guint) is
         begin
            Gtk_New (Description, D);
            Set_Line_Wrap (Description, True);
            Attach (Table, Description, v, b, l - 1, l,
            XPadding => Guint (OpenSteuer_Border_Width),
            YPadding => Guint (OpenSteuer_Border_Width));
            Set_Alignment (Description, 0.0, 0.0);
         end Make_Description;
         
         procedure Make_Eintrag (K, D : String; E : in out Num_Entry_Access; U : String; l : Guint) is
            Units : Gtk_Label;
         begin
            Make_Kuerzel (K, l);
            Make_Description (D, l, 1, 4);
            Gtk_New (E);
            Attach (Table, E, 5, 6, l - 1, l);
            Gtk_New (Units, U);
            Attach (Table, Units, 6, 7, l - 1, l,
            XPadding => Guint (OpenSteuer_Border_Width));
         end Make_Eintrag;
         
         procedure Make_Einfachmodus (Seite : Notebook_Pages) is
         begin
            Gtk_New (Kuerzel, "Geburtsjahr");
            Attach (Table, Kuerzel, 0, 2, 0, 1,
            XPadding => Guint (OpenSteuer_Border_Width));
            Set_Alignment (Kuerzel, 0.0, 0.5);
            Gtk_New (Adjustment, Geburtsjahr, 1900.0, 2003.0, 1.0, 1.0, 0.0);
            Gtk_New (Lohnsteuer.ALTER1_Spin_Entry(Seite), Adjustment, 1.0, 0);
            Set_Numeric (Lohnsteuer.ALTER1_Spin_Entry(Seite), True);
            Attach (Table, Lohnsteuer.ALTER1_Spin_Entry(Seite), 5, 6, 0, 1,
            YPadding => Guint (OpenSteuer_Border_Width));
            
            Make_Kuerzel ("ZKF", 2);
            Make_Description ("Zahl der Kinderfreibeträge (eine Dezimalstelle, nur bei Steuerklassen I, II, III und IV).", 2, 1, 4);
            Gtk_New (Adjustment, 0.0, 0.0, 9.5, 0.5, 0.5, 0.0);
            Gtk_New (Lohnsteuer.ZKF_Spin_Entry(Seite), Adjustment, 0.5, 1);
            Set_Numeric (Lohnsteuer.ZKF_Spin_Entry(Seite), True);
            Set_Snap_To_Ticks (Lohnsteuer.ZKF_Spin_Entry(Seite), True);
            Set_Data (Lohnsteuer.ZKF_Spin_Entry(Seite), ZKF);
            Attach (Table, Lohnsteuer.ZKF_Spin_Entry(Seite), 5, 6, 1, 2);
            
            Make_Kuerzel ("LZZ", 3);
            Make_Description ("Lohnzahlungszeitraum", 3, 1, 2);
            Gtk_New (Lohnsteuer.LZZ_Jahr(Seite), Widget_SList.Null_List, "Jahr");
            Attach (Table, Lohnsteuer.LZZ_Jahr(Seite), 2, 3, 2, 3);
            Gtk_New (Lohnsteuer.LZZ_Monat(Seite), Group (Lohnsteuer.LZZ_Jahr(Seite)), "Monat");
            Attach (Table, Lohnsteuer.LZZ_Monat(Seite), 3, 4, 2, 3);
            Gtk_New (Lohnsteuer.LZZ_Woche(Seite), Group (Lohnsteuer.LZZ_Jahr(Seite)), "Woche");
            Attach (Table, Lohnsteuer.LZZ_Woche(Seite), 4, 5, 2, 3);
            Gtk_New (Lohnsteuer.LZZ_Tag(Seite), Group (Lohnsteuer.LZZ_Jahr(Seite)), "Tag");
            Attach (Table, Lohnsteuer.LZZ_Tag(Seite), 5, 6, 2, 3);
            
            Make_Eintrag ("RE4", "Steuerpflichtiger Arbeitslohn vor Berücksichtigung des Versorgungs-Freibetrags, " &
            "des Altersentlastungsbetrags und des auf der Lohnsteuerkarte für den Lohnzahlungszeitraum eingetragenen Freibetrags in EUR.", Lohnsteuer.RE4_Entry(Seite), "EUR", 4);
            Set_Data (Lohnsteuer.RE4_Entry(Seite), RE4);
            
         end Make_Einfachmodus;
         
         Scrolled_Window : Gtk_Scrolled_Window;
         Label : Gtk_Label;
         k : Guint;
      begin
         Gtk_New (Table, 0, 0, False);
         Make_Einfachmodus (1);
         Gtk_New (Label, "Einfacher Modus");
         Insert_Page (Lohnsteuer.Eingabe, Table, Label, 0);
         
         Gtk_New (Table, 0, 0, False);
         
         Make_Einfachmodus (2);
         for i in 1..11 loop
            k := Guint (i + 4); -- Offset wg. Einträgen in Make_Einfachmodus.
            case i is
               when 3 =>
                  Make_Eintrag ("HINZUR", "In der Lohnsteuerkarte des Arbeitnehmers eingetragener Hinzurechnungsbetrag für den Lohnzahlungszeitraum in EUR.", Lohnsteuer.HINZUR_Entry, "EUR", k);
                  Set_Data (Lohnsteuer.HINZUR_Entry, HINZUR);
               when 1 =>
                  Make_Eintrag ("JFREIB", "Jahresfreibetrag nach Massgabe der Eintragungen auf der Lohnsteuerkarte in EUR (ggf. 0).", Lohnsteuer.JFREIB_Entry, "EUR", k);
                  Set_Data (Lohnsteuer.JFREIB_Entry, JFREIB);
               when 4 =>
                  Make_Eintrag ("JHINZU", "Jahreshinzurechnungsbetrag in EUR (ggf. 0).", Lohnsteuer.JHINZU_Entry, "EUR", k);
                  Set_Data (Lohnsteuer.JHINZU_Entry, JHINZU);
               when 9 =>
                  Make_Eintrag ("JRE4", "Voraussichtlicher Jahresarbeitslohn ohne sonstige Bezüge und ohne Vergütung für mehrjährige Tätigkeit in EUR (ggf. 0) Anmerkung: Die Eingabe dieses Feldes ist erforderlich bei Eingabe »sonstiger Bezüge« " &
                  "über 150 EUR (Feld SONSTB) oder bei Eingabe der »Vergütung für mehrjährige Tätigkeit« (Feld VMT).", Lohnsteuer.JRE4_Entry, "EUR", k);
                  Set_Data (Lohnsteuer.JRE4_Entry, JRE4);
               when 10 =>
                  Make_Eintrag ("JVBEZ", "In JRE4 enthaltene Versorgungsbezüge in EUR (ggf. 0).", Lohnsteuer.JVBEZ_Entry, "EUR", k);
                  Set_Data (Lohnsteuer.JVBEZ_Entry, JVBEZ);
               when 6 =>
                  Make_Kuerzel ("KRV", k);
                  Make_Description ("Der Arbeitnehmer ist im Lohnzahlungszeitraum in der gesetzlichen Rentenversicherung versicherungsfrei und gehört zu den in § 10 c Abs. 3 EStG genannten Personen. " &
                  "Für die Zuordnung sind allein die dem Arbeitgeber ohnehin bekannten Tatsachen massgebend; zusätzliche Ermittlungen braucht der Arbeitgeber nicht anzustellen.", k, 1, 4);
                  Gtk_New (Lohnsteuer.KRV_1_Ja, Widget_SList.Null_List, "Ja");
                  Attach (Table, Lohnsteuer.KRV_1_Ja, 4, 5, k-1, k);
                  Gtk_New (Lohnsteuer.KRV_0_Nein, Group (Lohnsteuer.KRV_1_Ja), "Nein");
                  Attach (Table, Lohnsteuer.KRV_0_Nein, 5, 6, k-1, k);
               when 7 =>
                  Make_Eintrag ("SONSTB", "Sonstige Bezüge (ohne Vergütung aus mehrjähriger Tätigkeit) in EUR (ggf. 0).", Lohnsteuer.SONSTB_Entry, "EUR", k);
                  Set_Data (Lohnsteuer.SONSTB_Entry, SONSTB);
               when 5 =>
                  Make_Eintrag ("VBEZ", "In RE4 enthaltene Versorgungsbezüge in EUR (ggf. 0).", Lohnsteuer.VBEZ_Entry, "EUR", k);
                  Set_Data (Lohnsteuer.VBEZ_Entry, VBEZ);
               when 8 =>
                  Make_Eintrag ("VBS", "In SONSTB enthaltene Versorgungsbezüge in EUR (ggf. 0).", Lohnsteuer.VBS_Entry, "EUR", k);
                  Set_Data (Lohnsteuer.VBS_Entry, VBS);
               when 11 =>
                  Make_Eintrag ("VMT", "Vergütung für mehrjährige Tätigkeit in EUR (ggf. 0).", Lohnsteuer.VMT_Entry, "EUR", k);
                  Set_Data (Lohnsteuer.VMT_Entry, VMT);
               when 2 =>
                  Make_Eintrag ("WFUNDF", "In der Lohnsteuerkarte des Arbeitnehmers eingetragener Freibetrag für den Lohnzahlungszeitraum in EUR.", Lohnsteuer.WFUNDF_Entry, "EUR", k);
                  Set_Data (Lohnsteuer.WFUNDF_Entry, WFUNDF);
            end case;
         end loop;
         
         Gtk_New (Scrolled_Window);
         Set_Policy (Scrolled_Window, Policy_Never, Policy_Automatic);
         Add_With_Viewport (Scrolled_Window, Table);
         Set_Focus_HAdjustment (Table, Get_HAdjustment (Scrolled_Window));
         Set_Focus_VAdjustment (Table, Get_VAdjustment (Scrolled_Window));
         
         Gtk_New (Label, "Expertenmodus");
         Insert_Page (Lohnsteuer.Eingabe, Scrolled_Window, Label, 1);
         
      end Make_Eingabe;
      
      function Make_Ausgabe return Gtk_Scrolled_Window is
         Table : Gtk_Table;
         Label : Gtk_Label;
         Scrolled_Window : Gtk_Scrolled_Window;
      begin
         Gtk_New (Table, 7, 2, False);
         
         Gtk_New (Label, "Steuerklasse :");
         Set_Alignment (Label, 0.0, 0.5);
         Attach (Table, Label, 0, 1, 0, 1, XPadding => Guint (OpenSteuer_Border_Width));
         Gtk_New (Label, "I");
         Attach (Table, Label, 1, 2, 0, 1);
         Gtk_New (Label, "II");
         Attach (Table, Label, 2, 3, 0, 1, XPadding => Guint (OpenSteuer_Border_Width));
         Gtk_New (Label, "III");
         Attach (Table, Label, 3, 4, 0, 1);
         Gtk_New (Label, "IV");
         Attach (Table, Label, 4, 5, 0, 1, XPadding => Guint (OpenSteuer_Border_Width));
         Gtk_New (Label, "V");
         Attach (Table, Label, 5, 6, 0, 1);
         Gtk_New (Label, "VI");
         Attach (Table, Label, 6, 7, 0, 1, XPadding => Guint (OpenSteuer_Border_Width));
         
         Gtk_New (Label, "Lohnsteuer (EUR) :");
         Attach (Table, Label, 0, 1, 1, 2, XPadding => Guint (OpenSteuer_Border_Width));
         Set_Alignment (Label, 0.0, 0.5);
         for i in STKL_Type'Range loop
            Gtk_New (Lohnsteuer.LSTLZZ_Array(i));
            if i mod 2 = 0 then
               Attach (Table, Lohnsteuer.LSTLZZ_Array(i), Guint (i), Guint (i) + 1, 1, 2,
               XPadding => Guint (OpenSteuer_Border_Width));
            else
               Attach (Table, Lohnsteuer.LSTLZZ_Array(i), Guint (i), Guint (i) + 1, 1, 2);
            end if;
         end loop;
         
         Gtk_New (Label, "Sol.-Zuschlag (EUR) :");
         Attach (Table, Label, 0, 1, 2, 3, XPadding => Guint (OpenSteuer_Border_Width));
         Set_Alignment (Label, 0.0, 0.5);
         for i in STKL_Type'Range loop
            Gtk_New (Lohnsteuer.SOLZLZZ_Array(i));
            if i mod 2 = 0 then
               Attach (Table, Lohnsteuer.SOLZLZZ_Array(i), Guint (i), Guint (i) + 1, 2, 3,
               XPadding => Guint (OpenSteuer_Border_Width));
            else
               Attach (Table, Lohnsteuer.SOLZLZZ_Array(i), Guint (i), Guint (i) + 1, 2, 3);
            end if;
         end loop;
         
         Gtk_New (Label, "8 Prozent auf Lst. (EUR) :");
         Attach (Table, Label, 0, 1, 3, 4, XPadding => Guint (OpenSteuer_Border_Width));
         Set_Alignment (Label, 0.0, 0.5);
         for i in STKL_Type'Range loop
            Gtk_New (Lohnsteuer.K8_Array(i));
            if i mod 2 = 0 then
               Attach (Table, Lohnsteuer.K8_Array(i), Guint (i), Guint (i) + 1, 3, 4,
               XPadding => Guint (OpenSteuer_Border_Width));
            else
               Attach (Table, Lohnsteuer.K8_Array(i), Guint (i), Guint (i) + 1, 3, 4);
            end if;
         end loop;
         
         Gtk_New (Label, "9 Prozent auf Lst. (EUR) :");
         Attach (Table, Label, 0, 1, 4, 5, XPadding => Guint (OpenSteuer_Border_Width));
         Set_Alignment (Label, 0.0, 0.5);
         for i in STKL_Type'Range loop
            Gtk_New (Lohnsteuer.K9_Array(i));
            if i mod 2 = 0 then
               Attach (Table, Lohnsteuer.K9_Array(i), Guint (i), Guint (i) + 1, 4, 5,
               XPadding => Guint (OpenSteuer_Border_Width));
            else
               Attach (Table, Lohnsteuer.K9_Array(i), Guint (i), Guint (i) + 1, 4, 5);
            end if;
         end loop;
         
         Calculate (Lohnsteuer);
         
         Gtk_New (Scrolled_Window);
         Set_Policy (Scrolled_Window, Policy_Automatic, Policy_Never);
         Add_With_Viewport (Scrolled_Window, Table);
         return Scrolled_Window;
      end Make_Ausgabe;
      
      HBox : Gtk_Box;
      Save_Button,
      -- Esc_Button,
      Calc_Button,
      Opt_Button : Gtk_Button;
   begin
      Reset_PAP_Intern;
      Reset_PAP_Eingabe;
      Gtk_New (Lohnsteuer.Win);
      Gtk.Window.Set_Border_Width (Lohnsteuer.Win, Gint (OpenSteuer_Border_Width));
      Set_Title (Lohnsteuer.Win, "Lohnsteuerformel");
      Set_Default_Size (Lohnsteuer.Win, 0, 400);
      
      Gtk_New_VBox (Lohnsteuer.VBox, False);
      Add (Lohnsteuer.Win, Lohnsteuer.VBox);
      
      Gtk_New (Lohnsteuer.Eingabe);
      Make_Eingabe;
      Lohnsteuer.Ausgabe := Make_Ausgabe;
      
      Pack_Start (Lohnsteuer.VBox, Lohnsteuer.Eingabe, True, True);
      
      Gtk_New_HSeparator (Lohnsteuer.Separator1);
      Pack_Start (Lohnsteuer.VBox, Lohnsteuer.Separator1, False, False, Padding => Gint (OpenSteuer_Border_Width));
      
      Pack_Start (Lohnsteuer.VBox, Lohnsteuer.Ausgabe, False, False);
      
      Gtk_New_HSeparator (Lohnsteuer.Separator2);
      Pack_Start (Lohnsteuer.VBox, Lohnsteuer.Separator2, False, False, Padding => Gint (OpenSteuer_Border_Width));
      
      Gtk_New_HBox (HBox, False);
      Gtk_New (Opt_Button, " Ein-/Ausgabe vertauschen ");
      Gtk_New (Calc_Button, " Berechnen ");
      Gtk_New (Save_Button, " Schließen ");
      -- Gtk_New (Esc_Button, " Abbrechen ");
      Pack_Start (HBox, Opt_Button, False, False);
      Pack_End (HBox, Calc_Button, False, False);
      Pack_End (HBox, Save_Button, False, False, Padding => Gint (OpenSteuer_Border_Width));
      -- Pack_End (HBox, Esc_Button, False, False);
      Pack_Start (Lohnsteuer.VBox, HBox, False, False);
      
      My_Button_Handler.Connect (Opt_Button, "clicked", My_Button_Handler.To_Marshaller (Einstellungen'Access), Lohnsteuer);
      My_Button_Handler.Connect (Calc_Button, "clicked", My_Button_Handler.To_Marshaller (Handle_Calculate'Access), Lohnsteuer);
      
      Handlers.Object_Connect (Save_Button, "clicked", Handlers.To_Marshaller (Gtk.Widget.Destroy_Cb'Access), Lohnsteuer.Win);
      -- Handlers.Object_Connect (Esc_Button, "clicked", Handlers.To_Marshaller (Gtk.Widget.Destroy_Cb'Access), Lohnsteuer.Win);
      
      My_Switch_Page_Handler.Connect (Lohnsteuer.Eingabe, "switch_page",
      My_Switch_Page_Handler.To_Marshaller (Handle_Page_Switch'Access), Lohnsteuer);
      
      Show_All (Lohnsteuer.Win);
   end Initialize;
   
   procedure Handle_Calculate (Button : access Gtk_Button_Record'Class; Lohnsteuer : Lohnsteuer_Access) is
   begin
      Update_Variables (Lohnsteuer);
      Calculate (Lohnsteuer);
   end Handle_Calculate;
   
   procedure Handle_Page_Switch (Notebook : access Gtk_Notebook_Record'Class; Lohnsteuer : Lohnsteuer_Access) is
   begin
      Update_Variables (Lohnsteuer);
      Update_Entries (Lohnsteuer);
   end Handle_Page_Switch;
   
   procedure Update_Variables (Lohnsteuer : Lohnsteuer_Access) is
      Seite : Notebook_Pages;
      -- tmp wird hier gebraucht, da Get_Current_Page auch den Wert -1
      -- annehmen kann:
      tmp : Gint := Get_Current_Page (Lohnsteuer.Eingabe) + 1;
   begin
      if tmp in Gint (Notebook_Pages'First)..Gint (Notebook_Pages'Last) then
         Seite := Notebook_Pages (tmp);
         if Steuer_Jahr - 1 - Integer'Value (Get_Text (Lohnsteuer.ALTER1_Spin_Entry(Seite))) > 64 then
            ALTER1 := 1;
         else
            ALTER1 := 0;
         end if;
         Geburtsjahr := Get_Data (Lohnsteuer.ALTER1_Spin_Entry(Seite));
         HINZUR := Get_Data (Lohnsteuer.HINZUR_Entry);
         JFREIB := Get_Data (Lohnsteuer.JFREIB_Entry);
         JHINZU := Get_Data (Lohnsteuer.JHINZU_Entry);
         JRE4 := Get_Data (Lohnsteuer.JRE4_Entry);
         JVBEZ := Get_Data (Lohnsteuer.JVBEZ_Entry);
         if Get_Active (Lohnsteuer.KRV_1_Ja) then
            KRV := 1;
         else
            KRV := 0;
         end if;
         if Get_Active (Lohnsteuer.LZZ_Jahr(Seite)) then
            LZZ := 1;
         elsif Get_Active (Lohnsteuer.LZZ_Monat(Seite)) then
            LZZ := 2;
         elsif Get_Active (Lohnsteuer.LZZ_Woche(Seite)) then
            LZZ := 3;
         else
            LZZ := 4;
         end if;
         RE4 := Get_Data (Lohnsteuer.RE4_Entry(Seite));
         SONSTB := Get_Data (Lohnsteuer.SONSTB_Entry);
         VBEZ := Get_Data (Lohnsteuer.VBEZ_Entry);
         VBS := Get_Data (Lohnsteuer.VBS_Entry);
         VMT := Get_Data (Lohnsteuer.VMT_Entry);
         WFUNDF := Get_Data (Lohnsteuer.WFUNDF_Entry);
         ZKF := Get_Data (Lohnsteuer.ZKF_Spin_Entry(Seite));
      end if;
   end Update_Variables;
   
   procedure Update_Entries (Lohnsteuer : Lohnsteuer_Access) is
   begin
      Set_Data (Lohnsteuer.HINZUR_Entry, HINZUR);
      Set_Data (Lohnsteuer.JFREIB_Entry, JFREIB);
      Set_Data (Lohnsteuer.JHINZU_Entry, JHINZU);
      Set_Data (Lohnsteuer.JRE4_Entry, JRE4);
      Set_Data (Lohnsteuer.JVBEZ_Entry, JVBEZ);
      case KRV is
         when 0 => Set_Active (Lohnsteuer.KRV_0_Nein, True);
         when 1 => Set_Active (Lohnsteuer.KRV_1_Ja, True);
      end case;
      Set_Data (Lohnsteuer.SONSTB_Entry, SONSTB);
      Set_Data (Lohnsteuer.VBEZ_Entry, VBEZ);
      Set_Data (Lohnsteuer.VBS_Entry, VBS);
      Set_Data (Lohnsteuer.VMT_Entry, VMT);
      Set_Data (Lohnsteuer.WFUNDF_Entry, WFUNDF);
      for i in Notebook_Pages'Range loop
         Set_Data (Lohnsteuer.ALTER1_Spin_Entry(i), Geburtsjahr);
         case LZZ is
            when 1 => Set_Active (Lohnsteuer.LZZ_Jahr(i), True);
            when 2 => Set_Active (Lohnsteuer.LZZ_Monat(i), True);
            when 3 => Set_Active (Lohnsteuer.LZZ_Woche(i), True);
            when 4 => Set_Active (Lohnsteuer.LZZ_Tag(i), True);
         end case;
         Set_Data (Lohnsteuer.RE4_Entry(i), RE4);
         Set_Data (Lohnsteuer.ZKF_Spin_Entry(i), ZKF);
      end loop;
   end Update_Entries;
   
   procedure Calculate (Lohnsteuer : Lohnsteuer_Access) is
   begin
      for i in STKL_Type'Range loop
         STKL := i;
         Reset_PAP_Intern;
         LST2002;
         Set_Text (Lohnsteuer.LSTLZZ_Array(i), Punkt_To_Komma (RW_2'Image (RW_2 (RW_Intern (LSTLZZ) / 100.0))));
         Set_Text (Lohnsteuer.SOLZLZZ_Array(i), Punkt_To_Komma (RW_2'Image (RW_2 (RW_Intern (SOLZLZZ) / 100.0))));
         Set_Text (Lohnsteuer.K8_Array(i), Punkt_To_Komma (RW_2'Image (RW_2 (RW_Intern (LSTLZZ) * 8.0 / 10000.0))));
         Set_Text (Lohnsteuer.K9_Array(i), Punkt_To_Komma (RW_2'Image (RW_2 (RW_Intern (LSTLZZ) * 9.0 / 10000.0))));
      end loop;
   end Calculate;
   
   procedure Einstellungen (Button : access Gtk_Button_Record'Class; Lohnsteuer : Lohnsteuer_Access) is
   begin
      Lohnsteuer.Ausgabe_Oben := not Lohnsteuer.Ausgabe_Oben;
      if Lohnsteuer.Ausgabe_Oben = True then
         Reorder_Child (Lohnsteuer.VBox, Lohnsteuer.Ausgabe, 0);
      else
         Reorder_Child (Lohnsteuer.VBox, Lohnsteuer.Eingabe, 0);
      end if;
      Reorder_Child (Lohnsteuer.VBox, Lohnsteuer.Separator1, 1);
   end Einstellungen;
   
   procedure Lohnsteuer is
      Lohnsteuer : Lohnsteuer_Access;
   begin
      Gtk_New (Lohnsteuer);
   end Lohnsteuer;
   
end OpenSteuer_LstTab;
