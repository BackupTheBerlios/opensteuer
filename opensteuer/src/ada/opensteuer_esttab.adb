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
--with Ada.Text_IO; use Ada.Text_IO;
with Gtk; use Gtk;
with Gtk.Window; use Gtk.Window;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Label; use Gtk.Label;
with Gdk.Event; use Gdk.Event;
with Gtk.Table; use Gtk.Table;
with Gtk.Separator; use Gtk.Separator;
with Gtk.Spin_Button; use Gtk.Spin_Button;
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

package body OpenSteuer_EstTab is
   
   ---------------------
   -- Einkommensteuer --
   ---------------------
   
   package Calc_Button_Handler is new Gtk.Handlers.User_Callback (Gtk_Button_Record, Einkommensteuer_Access);
   package Handlers is new Gtk.Handlers.Callback (Widget_Type => Gtk_Widget_Record);
   
   procedure Gtk_New (Einkommensteuer : out Einkommensteuer_Access) is
   begin
      Einkommensteuer := new Einkommensteuer_Record;
      Initialize (Einkommensteuer);
   end Gtk_New;
   
   procedure Initialize (Einkommensteuer : Einkommensteuer_Access) is
      Win : Gtk_Window;
      Label : Gtk_Label;
      Table : Gtk_Table;
      VBox, HBox : Gtk_Box;
      Separator : Gtk_Separator;
      Save_Button,
      -- Esc_Button,
      Calc_Button : Gtk_Button;
   begin
      Reset_PAP_Intern;
      Reset_PAP_Eingabe;
      Gtk_New (Win);
      Gtk.Window.Set_Border_Width (Win, Gint (OpenSteuer_Border_Width));
      Set_Title (Win, "Einkommensteuerformel");
      
      Gtk_New_VBox (VBox, False);
      Add (Win, VBox);
      
      Gtk_New (Table, 0, 0, False);
      
      Gtk_New (Label, "Est. Grundtabelle :");
      Set_Alignment (Label, 0.0, 0.5);
      Attach (Table, Label, 0, 1, 0, 1);
      Gtk_New (Einkommensteuer.ST_1);
      Set_Alignment (Einkommensteuer.ST_1, 1.0, 0.5);
      Attach (Table, Einkommensteuer.ST_1, 2, 3, 0, 1, XPadding => Guint (OpenSteuer_Border_Width));
      Gtk_New (Label, "EUR");
      Set_Alignment (Label, 0.0, 0.5);
      Attach (Table, Label, 3, 4, 0, 1);
      
      Gtk_New (Label, "Est. Splittingtabelle :");
      Set_Alignment (Label, 0.0, 0.5);
      Attach (Table, Label, 0, 1, 1, 2);
      Gtk_New (Einkommensteuer.ST_2);
      Set_Alignment (Einkommensteuer.ST_2, 1.0, 0.5);
      Attach (Table, Einkommensteuer.ST_2, 2, 3, 1, 2, XPadding => Guint (OpenSteuer_Border_Width));
      Gtk_New (Label, "EUR");
      Set_Alignment (Label, 0.0, 0.5);
      Attach (Table, Label, 3, 4, 1, 2);
      
      Rechne (Einkommensteuer);
      
      Pack_Start (VBox, Table, False, False);
      
      Gtk_New_HSeparator (Separator);
      Pack_Start (VBox, Separator, False, False, Padding => Gint (OpenSteuer_Border_Width));
      
      Gtk_New (Table, 0, 0, False);
      
      Gtk_New (Label, "ZVE");
      Attach (Table, Label, 0, 1, 0, 1);
      Set_Alignment (Label, 0.0, 0.5);
      Gtk_New (Label, "Zu versteuerndes Einkommen gem. § 32a Abs. 1 und 2 EStG");
      Set_Line_Wrap (Label, True);
      Attach (Table, Label, 1, 2, 0, 1,
      XPadding => Guint (OpenSteuer_Border_Width),
      YPadding => Guint (OpenSteuer_Border_Width));
      Set_Alignment (Label, 0.0, 0.0);
      Gtk_New (Einkommensteuer.ZVE_Entry);
      Set_Data (Einkommensteuer.ZVE_Entry, ZVE);
      Attach (Table, Einkommensteuer.ZVE_Entry, 2, 3, 0, 1, XPadding => Guint (OpenSteuer_Border_Width));
      Gtk_New (Label, "EUR");
      Attach (Table, Label, 3, 4, 0, 1);
      
      Pack_Start (VBox, Table, False, False);
      
      Gtk_New_HSeparator (Separator);
      Pack_Start (VBox, Separator, False, False, Padding => Gint (OpenSteuer_Border_Width));
      
      Gtk_New_HBox (HBox, False);
      Gtk_New (Calc_Button, " Berechnen ");
      Gtk_New (Save_Button, " Schließen ");
      -- Gtk_New (Esc_Button, " Abbrechen ");
      Pack_End (HBox, Calc_Button, False, False);
      Pack_End (HBox, Save_Button, False, False, Padding => Gint (OpenSteuer_Border_Width));
      -- Pack_End (HBox, Esc_Button, False, False);
      Pack_Start (VBox, HBox, False, False);
      
      Calc_Button_Handler.Connect (Calc_Button, "clicked",
      Calc_Button_Handler.To_Marshaller (Update'Access), Einkommensteuer);
      Handlers.Object_Connect (Save_Button, "clicked",
      Handlers.To_Marshaller (Gtk.Widget.Destroy_Cb'Access), Win);
      -- Handlers.Object_Connect (Esc_Button, "clicked",
      --    Handlers.To_Marshaller (Gtk.Widget.Destroy_Cb'Access), Win);
      
      Show_All (Win);
   end Initialize;
   
   procedure Update (Button : access Gtk_Button_Record'Class; Einkommensteuer : Einkommensteuer_Access) is
   begin
      ZVE := Get_Data (Einkommensteuer.ZVE_Entry);
      Rechne (Einkommensteuer);
   end Update;
   
   procedure Rechne (Einkommensteuer : Einkommensteuer_Access) is
   begin
      Einkommensteuer.ZVE_Intern := ZVE;
      Reset_PAP_Intern;
      STKL := 1;
      KZTAB := 1;
      ZVE := Einkommensteuer.ZVE_Intern;
      MLSTJAHR_2;
      Set_Text (Einkommensteuer.ST_1, Punkt_To_Komma (RW_2'Image (RW_2 (ST))));
      
      Reset_PAP_Intern;
      STKL := 1;
      KZTAB := 2;
      ZVE := Einkommensteuer.ZVE_Intern;
      MLSTJAHR_2;
      Set_Text (Einkommensteuer.ST_2, Punkt_To_Komma (RW_2'Image (RW_2 (ST))));
      
      ZVE := Einkommensteuer.ZVE_Intern;
   end Rechne;
   
   procedure Einkommensteuer is
      Einkommensteuer : Einkommensteuer_Access;
   begin
      Gtk_New (Einkommensteuer);
   end Einkommensteuer;
   
end OpenSteuer_EstTab;
