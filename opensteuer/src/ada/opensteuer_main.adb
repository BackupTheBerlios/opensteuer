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

with Glib; use Glib;
with Gtk; use Gtk;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Frame; use Gtk.Frame;
with OpenSteuer_Menu; use OpenSteuer_Menu;

package body OpenSteuer_Main is
   
   procedure Gtk_New (Main_Window : out Main_Window_Access) is
   begin
      Main_Window := new Main_Window_Record;
      OpenSteuer_Main.Initialize (Main_Window);
   end Gtk_New;
   
   procedure Initialize (Main_Window : Main_Window_Access) is
      Frame : Gtk_Frame;
   begin
      Gtk.Window.Initialize (Main_Window, Window_Toplevel);
      Set_Default_Size (Main_Window, 480, 360);
      Set_Title (Main_Window, "OpenSteuer");
      Set_Modal (Main_Window, False);
      
      Gtk_New_Vbox (Main_Window.Vbox, False, 0);
      Add (Main_Window, Main_Window.Vbox);
      
      Gtk_New (Main_Window.Menubar);
      Pack_Start (Main_Window.Vbox, Main_Window.Menubar, False, True, 0);
      Set_Shadow_Type (Main_Window.Menubar, Shadow_Out);
      
      Make_Menu_Datei (Main_Window.Menubar);
      Make_Menu_Tabellen (Main_Window.Menubar);
      Make_Menu_Hilfe (Main_Window.Menubar);
      
      Gtk_New (Frame);
      Pack_Start (Main_Window.Vbox, Frame, True, True, 0);
      
      Gtk_New (Main_Window.Statusbar);
      Pack_Start (Main_Window.Vbox, Main_Window.Statusbar, False, True, 0);
      
   end Initialize;
   
end OpenSteuer_Main;
