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

with Gtk.Menu; use Gtk.Menu;
with OpenSteuer_Menu_Handlers; use OpenSteuer_Menu_Handlers;

package body OpenSteuer_Menu is
   
   procedure Make_Menu_Datei (Menubar : in out Gtk_Menu_Bar) is
      Top : Gtk_Menu;
      Datei,
      --Speichern, Drucken, ELSTER, Separator,
      Beenden : Gtk_Menu_Item;
   begin
      Gtk_New (Datei, "Datei");
      Add (Menubar, Datei);
      Set_Right_Justify (Datei, False);
      
      Gtk_New (Top);
      Set_Submenu (Datei, Top);
      
      -- Gtk_New (Speichern, "Speichern");
      -- Add (Top, Speichern);
      -- Set_Right_Justify (Speichern, False);
      
      -- Gtk_New (Drucken, "Drucken");
      -- Add (Top, Drucken);
      -- Set_Right_Justify (Drucken, False);
      
      -- Gtk_New (ELSTER, "ELSTER");
      -- Add (Top, ELSTER);
      -- Set_Right_Justify (ELSTER, False);
      
      -- Gtk_New (Separator);
      -- Add (Top, Separator);
      -- Set_Right_Justify (Separator, False);
      
      Gtk_New (Beenden, "Beenden");
      Menu_Item_Callback.Connect (Beenden, "activate",
      Menu_Item_Callback.To_Marshaller (On_Quit_Activate'Access));
      Add (Top, Beenden);
      Set_Right_Justify (Beenden, False);
   end Make_Menu_Datei;
   
   procedure Make_Menu_Tabellen (Menubar : in out Gtk_Menu_Bar) is
      Top : Gtk_Menu;
      Tools, Lohnsteuer, Einkommensteuer : Gtk_Menu_Item;
   begin
      Gtk_New (Tools, "Tabellen");
      Add (Menubar, Tools);
      Set_Right_Justify (Tools, False);
      
      Gtk_New (Top);
      Set_Submenu (Tools, Top);
      
      Gtk_New (Lohnsteuer, "Lohnsteuer");
      Menu_Item_Callback.Connect (Lohnsteuer, "activate",
         Menu_Item_Callback.To_Marshaller (On_Lohnsteuer_Activate'Access));
      Add (Top, Lohnsteuer);
      Set_Right_Justify (Lohnsteuer, False);
      
      Gtk_New (Einkommensteuer, "Einkommensteuer");
      Menu_Item_Callback.Connect (Einkommensteuer, "activate",
         Menu_Item_Callback.To_Marshaller (
            On_Einkommensteuer_Activate'Access));
      Add (Top, Einkommensteuer);
      Set_Right_Justify (Einkommensteuer, False);
   end Make_Menu_Tabellen;
   
   procedure Make_Menu_Hilfe (Menubar : in out Gtk_Menu_Bar) is
      Top : Gtk_Menu;
      Hilfe, About : Gtk_Menu_Item;
   begin
      Gtk_New (Hilfe, "Hilfe");
      Add (Menubar, Hilfe);
      Set_Right_Justify (Hilfe, False);
      
      Gtk_New (Top);
      Set_Submenu (Hilfe, Top);
      
      Gtk_New (About, "About...");
      Menu_Item_Callback.Connect (About, "activate",
         Menu_Item_Callback.To_Marshaller (On_About_Activate'Access));
      Add (Top, About);
      Set_Right_Justify (About, False);
   end Make_Menu_Hilfe;
   
end OpenSteuer_Menu;
