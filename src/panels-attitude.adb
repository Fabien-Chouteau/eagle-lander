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
--    Eagle Lander is distributed in the hope that it will be useful,        --
--    but WITHOUT ANY WARRANTY; without even the implied warranty of         --
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the          --
--    GNU General Public License for more details.                           --
--                                                                           --
--    You should have received a copy of the GNU General Public License      --
--    along with Eagle Lander.  If not, see <http://www.gnu.org/licenses/>.  --
--                                                                           --
-------------------------------------------------------------------------------

with Ada.Numerics;
with Pango.Enums; use Pango.Enums;
with Text_Utils; use Text_Utils;

package body Panels.Attitude is

   type Rate_Panels is (Rate_Roll, Rate_Pitch, Rate_Yaw);

   procedure Draw_Ball (Cr : Cairo_Context; Radius : Gdouble);
   procedure Rate_Panel (Cr : Cairo_Context; Size : Gdouble;
                         Rate_Type : Rate_Panels);
   procedure Round_Tick (Cr : Cairo_Context;
                         Number : Positive;
                         Size : Gdouble);

   ---------------
   -- Draw_Ball --
   ---------------

   procedure Draw_Ball (Cr : Cairo_Context; Radius : Gdouble) is
   begin
      Arc (Cr, 0.0, 0.0, Radius, 0.0, 2.0 * Ada.Numerics.Pi);
      Clip (Cr);
      Set_Source_Rgb (Cr, 0.9, 0.9, 0.9);
      Arc (Cr, 0.0, 0.0, Radius, 0.0, 2.0 * Ada.Numerics.Pi);
      Fill (Cr);
      Set_Source_Rgb (Cr, 0.1, 0.1, 0.1);
      Arc (Cr, 0.0, 0.0, Radius, 0.0, Ada.Numerics.Pi);
      Fill (Cr);
      Set_Line_Width (Cr, Radius * 0.05);
      Set_Source_Rgba (Cr, 0.0, 0.0, 0.0, 0.3);
      Arc (Cr, 0.0, 0.0, Radius * 0.98, 0.0, 2.0 * Ada.Numerics.Pi);
      Stroke (Cr);

      Set_Line_Width (Cr, Radius * 0.03);
      Set_Dash (Cr, (Radius / 100.0, Radius / 10.0), 0.0);
      Set_Source_Rgb (Cr, 0.0, 0.0, 0.0);
      Move_To (Cr, Radius * 0.3, -Radius);
      Line_To (Cr, Radius * 0.3, 0.0);
      Stroke (Cr);
      Set_Source_Rgb (Cr, 1.0, 1.0, 1.0);
      Move_To  (Cr, Radius * 0.3, Radius);
      Line_To  (Cr, Radius * 0.3, 0.0);
      Stroke (Cr);

      Set_Line_Width (Cr, Radius * 0.03);
      Set_Dash (Cr, (Radius / 100.0, Radius / 10.0), 0.0);
      Set_Source_Rgb (Cr, 0.0, 0.0, 0.0);
      Move_To (Cr, -(Radius * 0.3), -Radius);
      Line_To (Cr, -(Radius * 0.3), 0.0);
      Move_To (Cr, -Radius, -(Radius * 0.3));
      Line_To (Cr, Radius, -(Radius * 0.3));
      Stroke (Cr);
      Set_Source_Rgb (Cr, 1.0, 1.0, 1.0);
      Move_To (Cr, -(Radius * 0.3), Radius);
      Line_To (Cr, -(Radius * 0.3), 0.0);
      Move_To (Cr, -Radius, Radius * 0.3);
      Line_To (Cr, Radius, Radius * 0.3);
      Stroke (Cr);
   end Draw_Ball;

   ----------------
   -- Rate_Panel --
   ----------------

   procedure Rate_Panel (Cr : Cairo_Context; Size : Gdouble;
                         Rate_Type : Rate_Panels)
   is
      procedure Tick (Number : Positive; Width, Tick_Size : Gdouble);

      ----------
      -- Tick --
      ----------

      procedure Tick (Number : Positive; Width, Tick_Size : Gdouble) is
         Width_T : constant Gdouble := Width / Gdouble ((Number - 1));
      begin
         for I in 0 .. Number - 1 loop
            Move_To (Cr, Width_T * Gdouble (I), 0.0);
            Line_To (Cr, Width_T * Gdouble (I), Tick_Size);
         end loop;
         Stroke (Cr);
      end Tick;
   begin
      Save (Cr);
      Set_Source_Rgb (Cr, 1.0, 1.0, 1.0);
      Rectangle (Cr, 0.0, 0.0, Size, Size / 13.0);
      Fill (Cr);
      Translate (Cr, Size * 0.05, Size / 13.0 + Size * 0.01);
      Tick (3, Size * 0.9, Size / 15.0);
      Tick (11, Size * 0.9, Size / 30.0);
      Tick (21, Size * 0.9, Size / 60.0);
      Restore (Cr);
      Set_Font_Size (Cr, Size / 15.0);
      Move_To (Cr, Size / 2.0, Size / 4.5);
      Draw_Centered_Text (Cr   => Cr,
                          Text => (case Rate_Type is
                                      when Rate_Roll => "ROLL RATE",
                                      when Rate_Pitch => "PITCH RATE",
                                      when Rate_Yaw => "YAW RATE"),
                          Pos  => (Size / 2.0, -Size / 28.0),
                          Size => Size / 14.0,
                          Gravity => (case Rate_Type is
                                      when Rate_Roll => Pango_Gravity_South,
                                      when Rate_Pitch => Pango_Gravity_East,
                                      when Rate_Yaw => Pango_Gravity_North));
   end Rate_Panel;

   ----------------
   -- Round_Tick --
   ----------------

   procedure Round_Tick (Cr : Cairo_Context;
                         Number : Positive;
                         Size : Gdouble) is
      Rotation : constant Gdouble := 2.0 * Ada.Numerics.Pi / Gdouble (Number);
   begin
      for I in 1 .. Number loop
         Rotate (Cr, Rotation);
         Move_To (Cr, 0.0, 0.0);
         Line_To (Cr, Size, 0.0);
         Stroke (Cr);
      end loop;
   end Round_Tick;

   ----------
   -- Init --
   ----------

   procedure Init (Self : in out Attitude_Indicator;
                   Pos  : Vector2D;
                   Size : Gdouble) is
   begin
      Self.Pos := Pos;
      Self.Size := (Size, Size);
   end Init;

   ----------
   -- Draw --
   ----------

   procedure Draw (Self       : in out Attitude_Indicator;
                   Cr         : Cairo_Context;
                   Pitch      : Angle;
                   Pitch_Rate : Angular_Velocity) is
      Size            : Gdouble;
      Center_X        : Gdouble;
      Center_Y        : Gdouble;
      Radius          : Gdouble;
      Tick_Size       : Gdouble;
      Red_Zone        : Gdouble;
      Rate_Gauge_Size : Gdouble;
      Cur_Pos         : Gdouble;
      A_Line          : Vector2D;
   begin
      Self.Draw_Frame (Cr);

      --  Real inner Size is only known after Draw_Frame()
      Size            := Self.Inner_Frame_Size.X;
      Center_X        := Size / 2.0;
      Center_Y        := Size / 2.0;
      Radius          := (Size / 2.0) * 0.6;
      Tick_Size       := Radius * 0.1;
      Red_Zone        := Ada.Numerics.Pi / 12.0;
      Rate_Gauge_Size := Size / 2.0;
      Cur_Pos         :=
        5.0 * Gdouble (Pitch_Rate) * (Rate_Gauge_Size * 0.9) / 2.0;

      --  Check cursor limits
      if Cur_Pos > 54.0 then
         Cur_Pos := 54.0;
      elsif Cur_Pos < -54.0 then
         Cur_Pos := -54.0;
      end if;

      Save (Cr);
      Translate (Cr, Self.Inner_Frame_Pos.X, Self.Inner_Frame_Pos.Y);
      Scale (Cr, Self.Scale, Self.Scale);

      --  Static background

      --  Black  background
      Save (Cr);
      Rectangle (Cr, 2.0, 2.0, Size - 4.0, Size - 4.0);
      Clip (Cr);
      Translate (Cr, Center_X, Center_Y);
      A_Line := Rotate ((0.0, 135.0), Ada.Numerics.Pi / 8.0);

      Move_To (Cr, A_Line.X, A_Line.Y);
      for Index in 1 .. 8 loop
         A_Line := Rotate (A_Line, Ada.Numerics.Pi / 4.0);
         Line_To (Cr, A_Line.X, A_Line.Y);
      end loop;
      Set_Source_Rgb (Cr, 0.0, 0.0, 0.0);
      Stroke_Preserve (Cr);
      Fill (Cr);
      Restore (Cr);

      --  Red zone
      Save (Cr);
      Translate (Cr, Center_X, Center_Y);
      Set_Source_Rgb (Cr, 1.0, 0.0, 0.0);
      Line_To (Cr, 0.0, 0.0);
      Arc (Cr, 0.0, 0.0, Radius + Tick_Size * 0.4, -Red_Zone, Red_Zone);
      Close_Path (Cr);
      Fill (Cr);
      Line_To (Cr, 0.0, 0.0);
      Arc (Cr, 0.0, 0.0, Radius + Tick_Size * 0.4,
           Ada.Numerics.Pi - Red_Zone, Ada.Numerics.Pi + Red_Zone);
      Close_Path (Cr);
      Fill (Cr);
      Restore (Cr);

      --  Ticks
      Set_Source_Rgb (Cr, 1.0, 1.0, 1.0);
      Save (Cr);
      Translate (Cr, Center_X, Center_Y);
      Round_Tick (Cr, 4, Radius + Tick_Size);
      Round_Tick (Cr, 12, Radius + Tick_Size * 0.8);
      Round_Tick (Cr, 36, Radius + Tick_Size * 0.6);
      Round_Tick (Cr, 72, Radius + Tick_Size * 0.4);
      Restore (Cr);

      --  Roll rate (top)
      Save (Cr);
      Translate (Cr, Center_X - Size / 4.0, Center_Y - Size / 2.2);
      Rate_Panel (Cr, Rate_Gauge_Size, Rate_Roll);
      Restore (Cr);
      --  Pitch rate (right)
      Save (Cr);
      Translate (Cr, Center_X + Size / 2.2, Center_Y - Size / 4.0);
      Rotate (Cr, Ada.Numerics.Pi / 2.0);
      Rate_Panel (Cr, Rate_Gauge_Size, Rate_Pitch);
      Restore (Cr);
      --  Yaw rate (bottom)
      Save (Cr);
      Translate (Cr, Center_X + Size / 4.0, Center_Y + Size / 2.2);
      Rotate (Cr, Ada.Numerics.Pi);
      Rate_Panel (Cr, Rate_Gauge_Size, Rate_Yaw);
      Restore (Cr);
      --  End of static background
      Save (Cr);
      Translate (Cr, Center_X, Center_Y);
      Rotate (Cr, Gdouble (Pitch));
      Draw_Ball (Cr, Radius);
      Restore (Cr);

      --  center cross
      Save (Cr);
      Translate (Cr, Center_X, Center_Y);
      Set_Line_Width (Cr, Radius * 0.01);
      Set_Dash (Cr, (1.0, 0.0), 0.0);
      Set_Source_Rgb (Cr, 0.0, 0.0, 0.0);
      Move_To (Cr, 0.0, -Radius);
      Line_To (Cr, 0.0,  Radius);
      Stroke (Cr);
      Move_To (Cr, -Radius, 0.0);
      Line_To (Cr, Radius, 0.0);
      Stroke (Cr);
      Set_Line_Width (Cr, Radius * 0.02);
      Set_Source_Rgb (Cr, 1.0, 0.75, 0.0);
      Move_To (Cr, 0.0, -Radius / 2.5);
      Line_To (Cr, 0.0,  Radius / 2.5);
      Stroke (Cr);
      Move_To (Cr, -Radius / 2.5, 0.0);
      Line_To (Cr, Radius / 2.5, 0.0);
      Stroke (Cr);
      Restore (Cr);

      Save (Cr);
      --  Roll rate (top)
      Translate (Cr, Center_X - Size / 4.0, Center_Y - Size / 2.2);
      Translate (Cr, Rate_Gauge_Size / 2.0, 0.0);
      Move_To (Cr, -Rate_Gauge_Size / 30.0, 0.0);
      Line_To (Cr, 0.0, Size / 30.0);
      Line_To (Cr, Rate_Gauge_Size / 30.0, 0.0);
      Close_Path (Cr);
      Set_Source_Rgb (Cr, 0.0, 0.0, 0.0);
      Fill (Cr);
      Restore (Cr);
      --  Pitch rate (right)
      Save (Cr);
      Translate (Cr, Center_X + Size / 2.2, Center_Y - Size / 4.0);
      Rotate (Cr, Ada.Numerics.Pi / 2.0);
      Translate (Cr, Rate_Gauge_Size / 2.0, 0.0);
      Move_To (Cr, -Rate_Gauge_Size / 30.0 - Cur_Pos, 0.0);
      Line_To (Cr, -Cur_Pos, Size / 30.0);
      Line_To (Cr, Rate_Gauge_Size / 30.0 - Cur_Pos, 0.0);
      Close_Path (Cr);
      Set_Source_Rgb (Cr, 0.0, 0.0, 0.0);
      Fill (Cr);
      Restore (Cr);
      --  Yaw rate (bottom)
      Save (Cr);
      Translate (Cr, Center_X + Size / 4.0, Center_Y + Size / 2.2);
      Rotate (Cr, Ada.Numerics.Pi);
      Translate (Cr, Rate_Gauge_Size / 2.0, 0.0);
      Move_To (Cr, -Rate_Gauge_Size / 30.0, 0.0);
      Line_To (Cr, 0.0, Size / 30.0);
      Line_To (Cr, Rate_Gauge_Size / 30.0, 0.0);
      Close_Path (Cr);
      Set_Source_Rgb (Cr, 0.0, 0.0, 0.0);
      Fill (Cr);
      Restore (Cr);

      Restore (Cr);
   end Draw;
end Panels.Attitude;
