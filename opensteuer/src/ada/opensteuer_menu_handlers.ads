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

with Gtk.Arguments;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Menu_Item; use Gtk.Menu_Item;

package OpenSteuer_Menu_Handlers is
   
   -----------
   -- Datei --
   -----------
   
   function On_Main_Window_Delete_Event (
      Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   return Boolean;
   
   procedure On_Quit_Activate (Object : access Gtk_Menu_Item_Record'Class);
   
   ---------------
   -- Berechnen --
   ---------------
   
   procedure On_Lohnsteuer_Activate (
      Object : access Gtk_Menu_Item_Record'Class);
   
   procedure On_Einkommensteuer_Activate (
      Object : access Gtk_Menu_Item_Record'Class);
   
   -----------
   -- Hilfe --
   -----------
   
   procedure On_About_Activate (
       Object : access Gtk_Menu_Item_Record'Class);

end OpenSteuer_Menu_Handlers;
