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

with Gdk.Event; use Gdk.Event;
with Glib; use Glib;
with Gtk.Main;
with Gtk.Handlers;
with OpenSteuer_Globals; use OpenSteuer_Globals;
with OpenSteuer_LstTab; use OpenSteuer_LstTab;
with OpenSteuer_EstTab; use OpenSteuer_EstTab;
with Gtk.Arguments; use Gtk.Arguments;
with Gtk.Window; use Gtk.Window;
with Gtk.Label; use Gtk.Label;
with Gtk.Box; use Gtk.Box;
with Gtk.Button; use Gtk.Button;
with Gtk.Separator; use Gtk.Separator;

package body OpenSteuer_Menu_Handlers is

   package Handlers is new Gtk.Handlers.Callback (Widget_Type => Gtk_Widget_Record);
   
   -----------
   -- Datei --
   -----------
   
   function On_Main_Window_Delete_Event (
      Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean is
      Arg1 : Gdk_Event := To_Event (Params, 1);
   begin
      Gtk.Main.Gtk_Exit (0);
      return True;
   end On_Main_Window_Delete_Event;
   
   procedure On_Quit_Activate (Object : access Gtk_Menu_Item_Record'Class) is
   begin
      Gtk.Main.Gtk_Exit (0);
   end On_Quit_Activate;
   
   --------------
   -- Tabellen --
   --------------
   
   procedure On_Lohnsteuer_Activate (
      Object : access Gtk_Menu_Item_Record'Class) is
   begin
      Lohnsteuer;
   end On_Lohnsteuer_Activate;
   
   procedure On_Einkommensteuer_Activate (
      Object : access Gtk_Menu_Item_Record'Class) is
   begin
      Einkommensteuer;
   end On_Einkommensteuer_Activate;
   
   -----------
   -- Hilfe --
   -----------
   
   procedure On_About_Activate (
      Object : access Gtk_Menu_Item_Record'Class) 
   is
      Window : Gtk_Window;
      Label : Gtk_Label;
      VBox : Gtk_Box;
      Ok_Button : Gtk_Button;
      Separator : Gtk_Separator;
   begin
      Gtk_New (Window);
      Set_Border_Width (Window, Gint (OpenSteuer_Border_Width));    
      Gtk_New_VBox (VBox, False);
      Add (Window, VBox);
      Gtk_New (Label, 
      "OpenSteuer" & ASCII.LF & 
      "Ein Open-Source-Steuerprogramm" & ASCII.LF & ASCII.LF & 
      "Version " & OpenSteuer_Version & ASCII.LF & ASCII.LF & 
      "Copyright (C) 2003 by" & ASCII.LF & 
      "Hannes Birnbacher" & ASCII.LF & 
      "Martin Klaiber" & ASCII.LF & 
      "Sigrid Wörsdörfer" & ASCII.LF & ASCII.LF & 
      "Dieses Programm unterliegt der GPL" & ASCII.LF & 
      "Eine Kopie der Lizenz befindet sich" & ASCII.LF & 
      "in der Datei COPYING");
      Pack_Start (VBox, Label, True, False);
      Gtk_New_HSeparator (Separator);
      Pack_Start (VBox, Separator, True, False,
         Padding => Gint (OpenSteuer_Border_Width));
      Gtk_New (Ok_Button, "   Ok   ");
      Pack_Start (VBox, Ok_Button, False, False);
      Handlers.Object_Connect (Ok_Button, "clicked",
         Handlers.To_Marshaller (Gtk.Widget.Destroy_Cb'Access), Window);
      Show_All (Window);
   end On_About_Activate;
   
end OpenSteuer_Menu_Handlers;
