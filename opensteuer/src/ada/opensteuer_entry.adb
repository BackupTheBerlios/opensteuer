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

-- with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Fixed;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Handlers; use Gtk.Handlers;
with Gtk.Arguments; use Gtk.Arguments;
with Gdk.Types; use Gdk.Types;
with Gdk.Types.KeySyms; use Gdk.Types.KeySyms;
with Gdk.Event; use Gdk.Event;
with OpenSteuer_Globals; use OpenSteuer_Globals;
-- with OpenSteuer_Debug; use OpenSteuer_Debug;

package body OpenSteuer_Entry is
   
   ---------------
   -- Num_Entry --
   ---------------
   
   package Check_Num_Keys_Callback is new Gtk.Handlers.Callback (Num_Entry_Record);
   package Check_Num_Keys_Return_Callback is new Gtk.Handlers.Return_Callback (Num_Entry_Record, Boolean);
   package Check_Num_Keys_User_Callback is new Gtk.Handlers.User_Callback (Num_Entry_Record, Gdk_Event);
   
   procedure Gtk_New (Num_Entry : out Num_Entry_Access) is
   begin
      Num_Entry := new Num_Entry_Record;
      OpenSteuer_Entry.Initialize (Num_Entry);
   end Gtk_New;
   
   procedure Initialize (Num_Entry : access Num_Entry_Record'Class) is
      Requisition : Gtk_Requisition;
   begin
      Gtk.GEntry.Initialize (Num_Entry);
      Check_Num_Keys_Return_Callback.Connect (Num_Entry, "key_press_event",
      Check_Num_Keys_Return_Callback.To_Marshaller (Check_Keys'Access));
      Requisition := Get_Child_Requisition (Num_Entry);
      Set_USize (Num_Entry, 100, Gint (Requisition.Height));
   end Initialize;
   
   -- Set_Data
   
   procedure Set_Data (Num_Entry : access Num_Entry_Record'Class; Data : Integer) is
   begin
      Set_Text (Num_Entry, Integer'Image (Data));
   end Set_Data;
   
   procedure Set_Data (Num_Entry : access Num_Entry_Record'Class; Data : Cent_0) is
   begin
      Set_Text (Num_Entry, Punkt_To_Komma (RW_2'Image (To_RW_2 (Data))));
   end Set_Data;
   
   procedure Set_Data (Num_Entry : access Num_Entry_Record'Class; Data : Euro_0) is
   begin
      Set_Text (Num_Entry, Punkt_To_Komma (RW_2'Image (RW_2 (Data))));
   end Set_Data;
   
   procedure Set_Data (Num_Entry : access Num_Entry_Record'Class; Data : RW_1) is
   begin
      Set_Text (Num_Entry, Punkt_To_Komma (RW_1'Image (Data)));
   end Set_Data;
   
   -- Get_Data
   
   function Get_Data (Num_Entry : access Num_Entry_Record'Class) return RW_Intern is
   begin
      return RW_Intern'Value (Komma_To_Punkt (Get_Text (Num_Entry)));
   end Get_Data;
   
   function Get_Data (Num_Entry : access Num_Entry_Record'Class) return Cent_0 is
   begin
      return To_Cent (Get_Data (Num_Entry));
   end Get_Data;
   
   function Get_Data (Num_Entry : access Num_Entry_Record'Class) return Euro_0 is
   begin
      return To_Euro (Get_Data (Num_Entry));
   end Get_Data;
   
   function Get_Data (Num_Entry : access Num_Entry_Record'Class) return RW_1 is
   begin
      return To_RW_1 (Get_Data (Num_Entry));
   end Get_Data;
   
   function Get_Data (Num_Entry : access Num_Entry_Record'Class) return Integer is
   begin
      return To_Integer (Get_Data (Num_Entry));
   end Get_Data;
   
   function Check_Keys (Num_Entry : access Num_Entry_Record'Class;
   Event : Gdk_Event_Key)
   return Boolean
   is
   Key : Gdk_Key_Type := Get_Key_Val (Event);
   Komma_Pos : Natural := Ada.Strings.Fixed.Index (Get_Text (Num_Entry), ",");
   Str_Len : Natural := Get_Text (Num_Entry)'Length;
   begin
      case Key is
         when GDK_0..GDK_9 =>
            if Komma_Pos > 0 then
               if Get_Position (Num_Entry) >= Gint (Komma_Pos) then
                  if Str_Len - Komma_Pos < 2 then
                     return False;
                  else
                     Emit_Stop_By_Name (Num_Entry, "key_press_event");
                     return True;
                  end if;
               end if;
            end if;
            return False;
         when GDK_Comma =>
            -- es darf nur ein Komma erlaubt sein:
            if Komma_Pos = 0 then
               return False;
            else
               Emit_Stop_By_Name (Num_Entry, "key_press_event");
               return True;
            end if;
         when GDK_Period =>
            Emit_Stop_By_Name (Num_Entry, "key_press_event");
            return True;
            -- Punkt in Komma umwandeln. Locale benutzen wäre besser.
            -- so geht's nicht:
            --      Key := GDK_Comma;
            --      Check_Keys_Callback.Emit_By_Name (Num_Entry, "key_press_event",  GDK_Comma);
            --      return False;
            --      when GDK_Left|GDK_Right|GDK_BackSpace|GDK_Delete =>
            --        return False;
         when others =>
            --        Emit_Stop_By_Name (Num_Entry, "key_press_event");
            --        return True;
            return False;
      end case;
   end Check_Keys;
   
   ----------------
   -- Spin_Entry --
   ----------------
   
   procedure Gtk_New (
      Spin_Entry  : out Spin_Entry_Access;
      Adjustment  : Gtk.Adjustment.Gtk_Adjustment;
      Climb_Rate  : in Gfloat;
      The_Digits  : in Gint) is
   begin
      Spin_Entry := new Spin_Entry_Record;
      OpenSteuer_Entry.Initialize (Spin_Entry, Adjustment, Climb_Rate, The_Digits);
   end Gtk_New;
   
   procedure Initialize (
      Spin_Entry  : access Spin_Entry_Record'Class;
      Adjustment  : Gtk.Adjustment.Gtk_Adjustment;
      Climb_Rate  : in Gfloat;
      The_Digits  : in Gint) is
   begin
      Gtk.Spin_Button.Initialize (Spin_Entry, Adjustment, Climb_Rate, The_Digits);
   end Initialize;
   
   -- Set_Data
   
   procedure Set_Data (Spin_Entry : access Spin_Entry_Record'Class; Data : Integer) is
   begin
      Set_Value (Spin_Entry, GFloat (Data));
   end Set_Data;
   
   procedure Set_Data (Spin_Entry : access Spin_Entry_Record'Class; Data : RW_1) is
   begin
      Set_Value (Spin_Entry, GFloat (Data));
   end Set_Data;
   
   procedure Set_Data (Spin_Entry : access Spin_Entry_Record'Class; Data : Gfloat) is
   begin
      Set_Value (Spin_Entry, Data);
   end Set_Data;
   
   -- Get_Data
   
   function Get_Data (Spin_Entry : access Spin_Entry_Record'Class) return Gfloat is
   begin
      return Get_Value_As_Float (Spin_Entry);
   end Get_Data;
   
   function Get_Data (Spin_Entry : access Spin_Entry_Record'Class) return RW_Intern is
   begin
      return RW_Intern (Get_Value_As_Float (Spin_Entry));
   end Get_Data;
   
   function Get_Data (Spin_Entry : access Spin_Entry_Record'Class) return RW_1 is
   begin
      return To_RW_1 (Get_Data (Spin_Entry));
   end Get_Data;
   
   function Get_Data (Spin_Entry : access Spin_Entry_Record'Class) return Cent_0 is
   begin
      return To_Cent (Get_Data (Spin_Entry));
   end Get_Data;
   
   function Get_Data (Spin_Entry : access Spin_Entry_Record'Class) return Integer is
   begin
      return To_Integer (Get_Data (Spin_Entry));
   end Get_Data;
   
end OpenSteuer_Entry;
