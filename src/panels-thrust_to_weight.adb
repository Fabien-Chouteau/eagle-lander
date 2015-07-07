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

with Text_Utils; use Text_Utils;

package body Panels.Thrust_To_Weight is

   ----------
   -- Init --
   ----------

   procedure Init (Self : in out TW_Panel;
                   Pos  : Vector2D;
                   Size : Vector2D) is
   begin
      Self.Pos := Pos;
      Self.Size := Size;
   end Init;

   ----------
   -- Draw --
   ----------

   procedure Draw (Self  : in out TW_Panel;
                   Cr    : Cairo_Context;
                   Value : Gdouble)
   is
      Size        : Vector2D;
      Margin_Side : Gdouble;
      Margin_Up   : Gdouble;
      Bar_Width   : Gdouble;
      Bar_Height  : Gdouble;
      H : Gdouble;
   begin
      Self.Draw_Frame (Cr);

      --  Real inner Size is only known after Draw_Frame()
      Size         := Self.Inner_Frame_Size;
      Margin_Side  := 0.26 * Size.X;
      Margin_Up    := 0.15 * Size.Y;
      Bar_Width    := 0.15 * Size.X;
      Bar_Height   := Size.Y - Margin_Up - Margin_Side;
      H := Margin_Up + (6.0 + Value) / 6.0 * Bar_Height * 0.9;

      Save (Cr);
      Translate (Cr, Self.Inner_Frame_Pos.X, Self.Inner_Frame_Pos.Y);
      Scale (Cr, Self.Scale, Self.Scale);

      --  static Background

      --  Black Background
      Set_Source_Rgb (Cr, 0.0, 0.0, 0.0);
      Rectangle (Cr,
                 Margin_Side * 0.5,
                 Margin_Up * 0.9,
                 Size.X - Margin_Side * 1.0,
                 Size.Y - Margin_Up * 0.8 - Margin_Side);
      Fill (Cr);

      Set_Source_Rgb (Cr, 1.0, 1.0, 1.0);

      Draw_Centered_Text (Cr, "T/W",
                          (X => Size.X / 2.0, Y => Margin_Up / 2.0),
                          Margin_Up * 0.5);
      --  Side Line
      Rectangle (Cr, Size.X - Margin_Side, Margin_Up,
                 -Bar_Width, Bar_Height);
      Fill (Cr);

      --  Draw white ticks on the scale
      declare
         Tick_Spacing : constant Gdouble := Bar_Height * 0.9 / 30.0;
         Tick_Y : Gdouble := Margin_Up + Bar_Height * 0.05;
         Tick_Width : Gdouble;
         Tick_X_R  : constant Gdouble := Size.X - Margin_Side - Bar_Width;
      begin
         for Cnt in 1 .. 31 loop
            Tick_Width :=
              (if Cnt mod 5 = 1 then Bar_Width else Bar_Width / 2.0);
            Move_To (Cr, Tick_X_R, Tick_Y);
            Line_To (Cr, Tick_X_R - Tick_Width, Tick_Y);
            Stroke (Cr);

            Tick_Y := Tick_Y + Tick_Spacing;
         end loop;
      end;

      --  Draw numbers on the scale
      declare
         Num_Spacing : constant Gdouble := Bar_Height * 0.9 / 30.0;
         Num_Y : Gdouble := Margin_Up + Bar_Height * 0.05;
         Num_X  : constant Gdouble := Size.X - Margin_Side - Bar_Width * 3.0;
      begin
         for Cnt in reverse 0 ..  6 loop
            Draw_Centered_Text (Cr, Integer'Image (Cnt),
                                (X => Num_X, Y => Num_Y),
                                Num_Spacing * 1.5);
            Num_Y := Num_Y + Num_Spacing * 5.0;
         end loop;
      end;
      --  End of static background

      --  Draw Cursor
      Set_Source_Rgb (Cr, 0.1, 0.1, 0.1);
      Move_To (Cr, Size.X - Margin_Side + 1.0, H);
      Line_To (Cr, Size.X - Margin_Side - Bar_Width,
               H + Bar_Height * 0.05);
      Line_To (Cr, Size.X - Margin_Side + 1.0, H + Bar_Height * 0.1);
      Close_Path (Cr);
      Fill (Cr);
      Restore (Cr);
   end Draw;

end Panels.Thrust_To_Weight;
