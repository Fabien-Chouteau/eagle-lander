-------------------------------------------------------------------------------
--                                                                           --
--                               Eagle Lander                                --
--                                                                           --
--         Copyright (C) 2015 Fabien Chouteau (chouteau@adacore.com)         --
--                                                                           --
--    Eagle Lander is free software: you can redistribute it and/or modify   --
--    it under the terms of the GNU General Public License as published by   --
--    the Free Software Foundation, either version 3 of the License, or      --
--    (at your option) any later version.                                    --
--                                                                           --
--    Foobar is distributed in the hope that it will be useful,              --
--    but WITHOUT ANY WARRANTY; without even the implied warranty of         --
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the          --
--    GNU General Public License for more details.                           --
--                                                                           --
--    You should have received a copy of the GNU General Public License      --
--    along with Eagle Lander.  If not, see <http://www.gnu.org/licenses/>.  --
--                                                                           --
-------------------------------------------------------------------------------

with Gtk; use Gtk;
with Gtk.Main;
with Gtk.Drawing_Area; use Gtk.Drawing_Area;
with Gtk.Handlers;
with Gtk.Window;
with Cairo; use Cairo;
with Gtk.Widget; use Gtk.Widget;
with Glib.Object; use Glib.Object;
with Gdk.Window; use Gdk.Window;
use Gdk;
with Gdk.Event; use Gdk.Event;
with Gdk.Types;
with Gdk.Types.Keysyms;
with Lander; use Lander;
with Gtk.Adjustment; use Gtk.Adjustment;
with Background;
with Timeline; use Timeline;
with Gtk.Toggle_Button; use Gtk.Toggle_Button;
with Gtk.Scale; use Gtk.Scale;
with Panels; use Panels;
with Panels.Attitude;
with Panels.Thrust_To_Weight;
with Panels.Altitude;
with Geom; use Geom;
with Ada.Containers.Doubly_Linked_Lists;
with Cairo.Png; use Cairo.Png;
with Cairo.Surface;
with System.Dim.Mks; use System.Dim.Mks;
with Physics; use Physics;
with Glib; use Glib;
with Glib.Main; use Glib.Main;
with Pango; use Pango;
with Pango.Layout; use Pango.Layout;
with Pango.Cairo;  -- use Pango.Cairo;
with System; use System;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Text_Utils; use Text_Utils;
with LEM_Drawing;

package body GUI is

   Darea : Gtk_Drawing_Area;
   Left_RCS_Adj, Right_RCS_Adj, DPS_Adj, Timeline_Adj : Gtk_Adjustment;
   Pause : Gtk_Toggle_Button;
   Help  : Gtk_Toggle_Button;
   Timeline_Scale : Gtk_Scale;

   package Panel_List_Pck is
     new Ada.Containers.Doubly_Linked_Lists (Panel_Access);

   Panel_List : Panel_List_Pck.List;

   Overview  : aliased Timeline.Overview_Panel;
   DPS_Gauge : aliased Panels.Gauge;
   RCS_Gauge : aliased Panels.Gauge;
   Xpointer  : aliased Panels.Xpointer;
   Attitude  : aliased Panels.Attitude.Attitude_Indicator;
   TW        : aliased Panels.Thrust_To_Weight.TW_Panel;
   Alt       : aliased Panels.Altitude.Alt_Panel;

   Zoom : Gdouble := 10.0;

   Insignia_Surface : Cairo_Surface := Null_Surface;

   package Widget_Keypress_Handlers is new Gtk.Handlers.Return_Callback
     (Gtk_Widget_Record, Boolean);

   function Key_Pressed_Handler (Widget : access Gtk_Widget_Record'Class;
                                 Event  : Gdk.Event.Gdk_Event)
                                 return Boolean;

   package Event_Cb is new Gtk.Handlers.Return_Callback
     (Gtk_Drawing_Area_Record, Boolean);

   function Window_Idle return Boolean;

   function Redraw (Area  : access Gtk_Drawing_Area_Record'Class;
                    Cr    : Cairo_Context) return Boolean;

   procedure Draw_Ending (Cr : Cairo_Context; Situ : Ending_Situation);

   --  Buttons handlers --
   procedure Pause_Toggled (Object : access Gtkada_Builder_Record'Class);
   procedure Reset (Object : access Gtkada_Builder_Record'Class);
   procedure DPS_Throttle_Change_Value
     (Object : access Gtkada_Builder_Record'Class);
   procedure RCS_Throttle_Change_Value
     (Object : access Gtkada_Builder_Record'Class);
   procedure Timeline_Change_Value
     (Object : access Gtkada_Builder_Record'Class);
   procedure Quit (Object : access Gtkada_Builder_Record'Class);

   function On_Button
     (Self  : access Gtk_Widget_Record'Class;
      Event : Gdk.Event.Gdk_Event_Button) return Boolean;

   function On_Motion
     (Self  : access Gtk_Widget_Record'Class;
      Event : Gdk.Event.Gdk_Event_Motion) return Boolean;

   -----------------
   -- Window_Idle --
   -----------------

   function Window_Idle return Boolean
   is
      W, H  : Gint;
   begin
      W := Darea.Get_Allocated_Width;
      H := Darea.Get_Allocated_Height;

      Invalidate_Rect (Get_Window (Darea),
                       (0, 0, W, H), Invalidate_Children => True);

      return True;
   end Window_Idle;

   -----------------
   -- Draw_Ending --
   -----------------

   procedure Draw_Ending (Cr : Cairo_Context; Situ : Ending_Situation) is
      Pos : constant Vector2D :=
        (Gdouble (Darea.Get_Allocated_Width) / 2.0,
         Gdouble (Darea.Get_Allocated_Height) / 5.0);
      Insigna_Pos : constant Vector2D :=
        (Gdouble (Darea.Get_Allocated_Width) / 2.0,
         Gdouble (Darea.Get_Allocated_Height) - Pos.Y);

      Insignia_Size : constant Gdouble := 700.0;
      Layout : Pango_Layout;
      Ink_Rect    : Pango_Rectangle;
      Logical_Rect : Pango_Rectangle;
      Scale : Gdouble;
   begin

      --  Draw insigna
      if Situ.Result = Sucess then
         if Insignia_Surface = Null_Surface then
            Insignia_Surface :=
              Create_From_Png ("resources/Apollo11.png");
         end if;

         --  If the image is available
         if Cairo.Surface.Status (Insignia_Surface) = Cairo_Status_Success then

            Save (Cr);

            --  Fit insigna to screen
            Scale := Gdouble (Darea.Get_Allocated_Height) / Insignia_Size;

            --  Take a third of that
            Scale := Scale / 3.0;
            Cairo.Translate (Cr,
                             Insigna_Pos.X - Insignia_Size / 2.0 * Scale,
                             Insigna_Pos.Y - Insignia_Size / 2.0 * Scale);
            Cairo.Scale (Cr, Scale, Scale);
            Set_Source_Surface (Cr, Insignia_Surface, 0.0, 0.0);
            Paint (Cr);
            Restore (Cr);
         end if;
      end if;

      Layout := LM_Font (Cr, 50.0);
      Set_Text (Layout, To_String (Situ.Message));
      Layout.Get_Pixel_Extents (Ink_Rect, Logical_Rect);

      Save (Cr);

      --  Background
      Rectangle (Cr     => Cr,
                 X      => Pos.X - Gdouble ((Logical_Rect.Width + 10) / 2),
                 Y      => Pos.Y - Gdouble ((Logical_Rect.Height + 10) / 2),
                 Width  => Gdouble (Logical_Rect.Width + 10),
                 Height => Gdouble (Logical_Rect.Height + 10));

      if Situ.Result = Sucess then
         Set_Source_Rgb (Cr, 1.0, 1.0, 1.0);
      else
         Set_Source_Rgb (Cr, 1.0, 0.0, 0.0);
      end if;
      Fill_Preserve (Cr);

      if Situ.Result = Sucess then
         Set_Source_Rgb (Cr, 0.0, 0.0, 0.0);
      else
         Set_Source_Rgb (Cr, 1.0, 1.0, 1.0);
      end if;

      --  Box
      Set_Line_Width (Cr, 2.0);
      Stroke (Cr);

      --  Text
      Move_To (Cr, Pos.X - Gdouble (Logical_Rect.Width / 2),
               Pos.Y - Gdouble (Logical_Rect.Height / 2));

      Pango.Cairo.Show_Layout  (Cr, Layout);
      Restore (Cr);
   end Draw_Ending;

   ------------
   -- Redraw --
   ------------

   function Redraw (Area  : access Gtk_Drawing_Area_Record'Class;
                    Cr    : Cairo_Context) return Boolean is
      pragma Unreferenced (Area);
   begin
      Cairo.Save (Cr);
      --  Set the origin at the bottom left corner
      Cairo.Scale (Cr, 1.0, -1.0);
      Cairo.Translate (Cr, 0.0, Gdouble (-Darea.Get_Allocated_Height));

      Cairo.Translate (Cr, Gdouble (Darea.Get_Allocated_Width) / 2.0,
                       Gdouble (Darea.Get_Allocated_Height) / 2.0);

      Cairo.Scale (Cr, Zoom, Zoom);
      Cairo.Translate (Cr, -Gdouble (Lander.Get_Situation.Pos.X),
                       -Gdouble (Lander.Get_Situation.Pos.Y));
      if not Pause.Get_Active and then not Lander.Are_We_Done_Yet then
         Lander.Phys_Step (s / Dimentionless (Simulation_Frequency));
         Timeline_Adj.Set_Upper (Gdouble (Timeline.Lenght));
         Timeline_Adj.Set_Value (Gdouble (Timeline.Lenght));
      end if;

      Background.Draw (Cr);
      if Help.Get_Active and then not Lander.Are_We_Done_Yet then
         LEM_Drawing.Draw_Forecast_And_Speed_Vect
           (Cr, Lander.Get_Situation, 0.5 * s, 50);
      end if;

      LEM_Drawing.Draw (Cr, Lander.Get_Situation);

      Cairo.Restore (Cr);

      Alt.Draw (Cr, Lander.Get_Situation.Pos.Y,
                    Lander.Get_Situation.Vel.Y);
      TW.Draw (Cr, Lander.Get_Thrust_To_Weight);
      if Help.Get_Active then
         Overview.Draw (Cr);
      end if;
      Attitude.Draw (Cr, Lander.Get_Situation.Pitch,
                     Lander.Get_Situation.Pitch_R);
      Xpointer.Draw (Cr, Lander.Get_Situation.Vel);
      DPS_Gauge.Draw (Cr, Lander.Get_DPS_Propellent_Level);
      RCS_Gauge.Draw (Cr, Lander.Get_RCS_Propellent_Level);

      if Lander.Are_We_Done_Yet then
         Draw_Ending (Cr, Lander.How_did_it_end);
         Pause.Set_Active (True);
      end if;

      return False;
   end Redraw;

   -------------------
   -- Pause_Toggled --
   -------------------

   procedure Pause_Toggled (Object : access Gtkada_Builder_Record'Class) is
      pragma Unreferenced (Object);
   begin
      if not Pause.Get_Active then
         Timeline.Set_Now (Simulation_Time (Timeline_Adj.Get_Value));
         Timeline_Scale.Set_Sensitive (False);
      else
         Timeline_Scale.Set_Sensitive (True);
      end if;
   end Pause_Toggled;

   ----------
   -- Quit --
   ----------

   procedure Quit (Object : access Gtkada_Builder_Record'Class) is
      pragma Unreferenced (Object);
   begin
      Gtk.Main.Main_Quit;
   end Quit;

   -----------
   -- Reset --
   -----------

   procedure Reset (Object : access Gtkada_Builder_Record'Class) is
      pragma Unreferenced (Object);
      Situ  : constant Lander.Lander_Situation := Lander.Get_Situation;
   begin
      Lander.Reset;
      Timeline_Adj.Set_Value (1.0);
      Timeline.Set_Now (0);
      Timeline.Set_Situation (Lander.Get_Situation);

      DPS_Adj.Set_Value (Gdouble (Situ.DPS_Throttle) * 100.0);
      Left_RCS_Adj.Set_Value (Gdouble (Situ.Left_RCS_Throttle) * 100.0);
      Right_RCS_Adj.Set_Value (Gdouble (Situ.Right_RCS_Throttle) * 100.0);
   end Reset;

   -------------------------------
   -- DPS_Throttle_Change_Value --
   -------------------------------

   procedure DPS_Throttle_Change_Value
     (Object : access Gtkada_Builder_Record'Class)
   is
      pragma Unreferenced (Object);
   begin
      Set_DPS_Throttle (DPS_Adj.Get_Value / 100.0);

      DPS_Adj.Set_Value (Gdouble (Get_Situation.DPS_Throttle) * 100.0);
   end DPS_Throttle_Change_Value;

   -------------------------------
   -- RCS_Throttle_Change_Value --
   -------------------------------

   procedure RCS_Throttle_Change_Value
     (Object : access Gtkada_Builder_Record'Class)
   is
      pragma Unreferenced (Object);
   begin
      --  Only Left is user adjustable, Right is just the oposite
      Right_RCS_Adj.Set_Value (-Left_RCS_Adj.Get_Value);

      Set_Left_RCS_Throttle (Left_RCS_Adj.Get_Value / 100.0);
      Set_Right_RCS_Throttle (-Left_RCS_Adj.Get_Value / 100.0);
   end RCS_Throttle_Change_Value;

   ---------------------------
   -- Timeline_Change_Value --
   ---------------------------

   procedure Timeline_Change_Value
     (Object : access Gtkada_Builder_Record'Class)
   is
      pragma Unreferenced (Object);
      Situ : constant Lander.Lander_Situation :=
        Timeline.Get_Situation (Simulation_Time (Timeline_Adj.Get_Value));
   begin
      Lander.Set_Situation (Situ);
      DPS_Adj.Set_Value (Gdouble (Situ.DPS_Throttle) * 100.0);
      Left_RCS_Adj.Set_Value (Gdouble (Situ.Left_RCS_Throttle) * 100.0);
      Right_RCS_Adj.Set_Value (Gdouble (Situ.Right_RCS_Throttle) * 100.0);

   end Timeline_Change_Value;

   -------------------------
   -- Key_Pressed_Handler --
   -------------------------

   function Key_Pressed_Handler
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk.Event.Gdk_Event)
      return Boolean
   is
      pragma Unreferenced (Widget);
      Key : constant Gdk.Types.Gdk_Key_Type := Gdk.Event.Get_Key_Val (Event);
   begin
      case Key is
         when Gdk.Types.Keysyms.GDK_Page_Up =>
            if Zoom < 250.0 then
               Zoom := Zoom * 5.0;
            end if;
         when Gdk.Types.Keysyms.GDK_Page_Down =>
            if Zoom > 2.0 then
               Zoom := Zoom / 5.0;
            end if;
         when Gdk.Types.Keysyms.GDK_Up =>
            DPS_Adj.Set_Value (DPS_Adj.Get_Value + 10.0);
         when Gdk.Types.Keysyms.GDK_Down =>
            DPS_Adj.Set_Value (DPS_Adj.Get_Value - 10.0);
         when Gdk.Types.Keysyms.GDK_Left =>
            Left_RCS_Adj.Set_Value (Left_RCS_Adj.Get_Value - 10.0);
         when Gdk.Types.Keysyms.GDK_Right =>
            Left_RCS_Adj.Set_Value (Left_RCS_Adj.Get_Value + 10.0);
         when Gdk.Types.Keysyms.GDK_space =>
            --  Do not reset ending at the last point of simulation because at
            --  this point the lander is at an ending position...
            if Simulation_Time (Timeline_Adj.Get_Value) /= Timeline.Lenght
              and then Timeline.Lenght /= 1
            then
               Lander.Reset_Ending;
            end if;
            Pause.Set_Active (not Pause.Get_Active);

         when others        => null;
      end case;

      return True;
   end Key_Pressed_Handler;

   ---------------
   -- On_Button --
   ---------------

   function On_Button
     (Self  : access Gtk_Widget_Record'Class;
      Event : Gdk.Event.Gdk_Event_Button) return Boolean is
      pragma Unreferenced (Self);

      W : constant Gdouble := Gdouble (Darea.Get_Allocated_Width);
      H : constant Gdouble := Gdouble (Darea.Get_Allocated_Height);
      X : Gdouble := Event.X;
      Y : Gdouble := Event.Y;
   begin
      if X <= W and then Y <= H then
         --  Align on grid
         X := Gdouble'Floor (X / 10.0) * 10.0;
         Y := Gdouble'Floor (Y / 10.0) * 10.0;

         if Event.The_Type = Button_Press then
            for P of Panel_List loop
               if Event.Button = 1 then
                  if P.On_Click ((X, Y)) then
                     return False;
                  end if;
               elsif Event.Button = 3 then
                  if P.On_Resize ((X, Y)) then
                     return False;
                  end if;
               end if;
            end loop;
         elsif Event.The_Type = Button_Release
           and then (Event.Button = 1 or else Event.Button = 3)
         then
            for P of Panel_List loop
               P.On_Released ((X, Y));
            end loop;
         end if;
      end if;
      return False;
   end On_Button;

   ---------------
   -- On_Motion --
   ---------------

   function On_Motion
     (Self  : access Gtk_Widget_Record'Class;
      Event : Gdk.Event.Gdk_Event_Motion) return Boolean is
      pragma Unreferenced (Self);

      W  : constant Gdouble := Gdouble (Darea.Get_Allocated_Width);
      H  : constant Gdouble := Gdouble (Darea.Get_Allocated_Height);
      X : Gdouble := Event.X;
      Y : Gdouble := Event.Y;
   begin
      if X <= W and then Y <= H then
         --  Align on grid
         X := Gdouble'Floor (X / 10.0) * 10.0;
         Y := Gdouble'Floor (Y / 10.0) * 10.0;

         for P of Panel_List loop
            P.On_Motion ((X, Y));
         end loop;
      end if;
      return False;
   end On_Motion;

   --------------
   -- Init_Gtk --
   --------------

   procedure Init_Gtk (Builder : Gtkada_Builder) is
      Src_Id : G_Source_Id;
      Main_W : Gtk.Window.Gtk_Window;
      pragma Unreferenced (Src_Id);
   begin

      RCS_Gauge.Init
        ("RCS", (X => 10.0, Y => 10.0), (X => 120.0, Y => 240.0));
      DPS_Gauge.Init
        ("DPS", (X => 130.0, Y => 10.0), (X => 120.0, Y => 240.0));
      Xpointer.Init ((X => 10.0, Y => 250.0), 240.0);
      Attitude.Init ((X => 10.0, Y => 490.0), 240.0);
      Overview.Init ((X => 250.0, Y => 10.0), (X => 480.0, Y => 240.0));
      TW.Init ((X => 370.0, Y => 250.0), (X => 60.0, Y => 240.0));
      Alt.Init ((X => 250.0, Y => 250.0), (X => 120.0, Y => 240.0));

      Panel_List.Append (RCS_Gauge'Access);
      Panel_List.Append (DPS_Gauge'Access);
      Panel_List.Append (Xpointer'Access);
      Panel_List.Append (Attitude'Access);
      Panel_List.Append (Overview'Access);
      Panel_List.Append (TW'Access);
      Panel_List.Append (Alt'Access);

      Left_RCS_Adj   := Gtk_Adjustment (Builder.Get_Object ("left_rcs_adj"));
      Right_RCS_Adj  := Gtk_Adjustment (Builder.Get_Object ("right_rcs_adj"));
      DPS_Adj        := Gtk_Adjustment (Builder.Get_Object ("dps_adj"));
      Timeline_Adj   := Gtk_Adjustment (Builder.Get_Object ("timeline_adj"));
      Pause          := Gtk_Toggle_Button (Builder.Get_Object ("pause"));
      Help           := Gtk_Toggle_Button (Builder.Get_Object ("help_toggle"));
      Timeline_Scale := Gtk_Scale (Builder.Get_Object ("timeline_scale"));

      Register_Handler (Builder, "Main_Quit",     Quit'Access);
      Register_Handler (Builder, "pause_toggled", Pause_Toggled'Access);
      Register_Handler (Builder, "DPS_throttle_change_value",
                        DPS_Throttle_Change_Value'Access);
      Register_Handler (Builder, "RCS_throttle_change_value",
                        RCS_Throttle_Change_Value'Access);
      Register_Handler (Builder, "timeline_change_value",
                        Timeline_Change_Value'Access);
      Register_Handler (Builder, "reset_handler",     Reset'Access);

      Darea := Gtk_Drawing_Area (Builder.Get_Object ("drawingarea"));
      --  Darea.Set_Size_Request (Gint (Geomaps.Square_Size * 11),
      --                          Gint (Geomaps.Square_Size * 11));
      Event_Cb.Connect (Darea, Signal_Draw,
                        Event_Cb.To_Marshaller (Redraw'Unrestricted_Access));

      Main_W := Gtk.Window.Gtk_Window (Get_Object (Builder, "Station"));

      Main_W.Add_Events (Button_Release_Mask);
      Main_W.Add_Events (Button_Press_Mask);
      Main_W.Add_Events (Pointer_Motion_Mask);
      Main_W.Add_Events (Pointer_Motion_Hint_Mask);
      Main_W.On_Button_Press_Event (On_Button'Access, True);
      Main_W.On_Button_Release_Event (On_Button'Access, True);
      Main_W.On_Motion_Notify_Event (On_Motion'Access);

      Widget_Keypress_Handlers.Connect
        (Main_W, Signal_Key_Press_Event,
         Widget_Keypress_Handlers.Event_Marshaller.To_Marshaller
           (Key_Pressed_Handler'Access));

      Do_Connect (Builder);

      Src_Id := Timeout_Add (1000 / Simulation_Frequency, Window_Idle'Access);

      Main_W.Show_All;
      Main_W.Maximize;

   end Init_Gtk;

end GUI;
