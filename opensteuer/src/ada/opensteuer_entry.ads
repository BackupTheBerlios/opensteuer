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

with Gtk.Widget; use Gtk.Widget;
with Gtk.GEntry; use Gtk.GEntry;
with Gtk.Spin_Button; use Gtk.Spin_Button;
with OpenSteuer_Globals; use OpenSteuer_Globals;
with Gdk.Event; use Gdk.Event;
with Gtk.Adjustment;
with Glib; use Glib;

package OpenSteuer_Entry is
   
   ---------------
   -- Num_Entry --
   ---------------
   
   type Num_Entry_Record is new Gtk_Entry_Record with null record;
   
   type Num_Entry_Access is access all Num_Entry_Record'Class;
   
   procedure Gtk_New (Num_Entry : out Num_Entry_Access);
   procedure Initialize (Num_Entry : access Num_Entry_Record'Class);
   
   procedure Set_Data (Num_Entry : access Num_Entry_Record'Class; Data : Integer);
   procedure Set_Data (Num_Entry : access Num_Entry_Record'Class; Data : Cent_0);
   procedure Set_Data (Num_Entry : access Num_Entry_Record'Class; Data : Euro_0);
   procedure Set_Data (Num_Entry : access Num_Entry_Record'Class; Data : RW_1);
   
   function Get_Data (Num_Entry : access Num_Entry_Record'Class) return RW_1;
   function Get_Data (Num_Entry : access Num_Entry_Record'Class) return Cent_0;
   function Get_Data (Num_Entry : access Num_Entry_Record'Class) return Euro_0;
   function Get_Data (Num_Entry : access Num_Entry_Record'Class) return Integer;
   
   function Check_Keys (
      Num_Entry : access Num_Entry_Record'Class;
      Event     : Gdk_Event_Key)
      return boolean;
      
   ----------------
   -- Spin_Entry --
   ----------------
   
   type Spin_Entry_Record is new Gtk_Spin_Button_Record with null record;
   
   type Spin_Entry_Access is access all Spin_Entry_Record'Class;
   
   procedure Gtk_New (
      Spin_Entry : out Spin_Entry_Access;
      Adjustment : Gtk.Adjustment.Gtk_Adjustment;
      Climb_Rate : in Gfloat;
      The_Digits : in Gint);
      
   procedure Initialize (
      Spin_Entry : access Spin_Entry_Record'Class;
      Adjustment : Gtk.Adjustment.Gtk_Adjustment;
      Climb_Rate : in Gfloat;
      The_Digits : in Gint);
      
   procedure Set_Data (Spin_Entry : access Spin_Entry_Record'Class; Data : RW_1);
   procedure Set_Data (Spin_Entry : access Spin_Entry_Record'Class; Data : Gfloat);
   
   function Get_Data (Spin_Entry : access Spin_Entry_Record'Class) return RW_Intern;
   function Get_Data (Spin_Entry : access Spin_Entry_Record'Class) return RW_1;
   function Get_Data (Spin_Entry : access Spin_Entry_Record'Class) return Gfloat;
   
end OpenSteuer_Entry;
