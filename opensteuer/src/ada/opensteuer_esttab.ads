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
with Gtk.Label; use Gtk.Label;
with OpenSteuer_Globals; use OpenSteuer_Globals;
with OpenSteuer_Entry; use OpenSteuer_Entry;

package OpenSteuer_EstTab is
   
   procedure Einkommensteuer;
   
private
   
   type Einkommensteuer_Record is new Gtk_Window_Record with
      record
         ZVE_Intern : Euro_0;
         ZVE_Entry  : Num_Entry_Access;
         ST_1       : Gtk_Label;
         ST_2       : Gtk_Label;
      end record;
   
   type Einkommensteuer_Access is access all Einkommensteuer_Record'Class;
   
   procedure Gtk_New (Einkommensteuer : out Einkommensteuer_Access);
   procedure Initialize (Einkommensteuer : Einkommensteuer_Access);
   procedure Update (
      Button          : access Gtk_Button_Record'Class;
      Einkommensteuer : Einkommensteuer_Access);
   procedure Rechne (Einkommensteuer : Einkommensteuer_Access);
      
end OpenSteuer_EstTab;
