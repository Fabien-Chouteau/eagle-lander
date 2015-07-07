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

package body Panels.Altitude is

   ----------
   -- Init --
   ----------

   procedure Init
     (Self : in out Alt_Panel;
      Pos  : Vector2D;
      Size : Vector2D)
   is
   begin
      Self.Pos := Pos;
      Self.Size := Size;

   end Init;

   ----------
   -- Draw --
   ----------

   procedure Draw
     (Self : in out Alt_Panel;
      Cr   : Cairo_Context;
      Alt  : System.Dim.Mks.Length;
      Rate : Speed)
   is
      Size         : Vector2D;
      Title_Height : Gdouble;
      Text_Size    : Gdouble;
      Tape_Width   : Gdouble;

      Rate_Step_Height : constant Gdouble := 8.0;
      Rate_Max : constant Gdouble := 300.0; --  m/s
      Rate_Val : Gdouble := Gdouble (Rate);

      Alt_Step_Height : constant Gdouble := 20.0;
      Alt_Max : constant Gdouble := 5000.0;
      Alt_Val : Gdouble := Gdouble (Alt);

      procedure Draw_Rate_Tape;
      procedure Draw_Alt_Tape;

      --------------------
      -- Draw_Rate_Tape --
      --------------------

      procedure Draw_Rate_Tape is
         Tick_Width : Gdouble;
         Text_Size : constant Gdouble := Size.X * 0.06;
      begin

         if Rate_Val < -Rate_Max then
            Rate_Val := -Rate_Max;
         elsif Rate_Val > Rate_Max then
            Rate_Val := Rate_Max;
         end if;
         if Alt_Val < 0.0 then
            Alt_Val := 0.0;
         elsif Alt_Val > Alt_Max then
            Alt_Val := Alt_Max;
         end if;

         --  Below 0 the tape is white
         Set_Source_Rgb (Cr, 1.0, 1.0, 1.0);
         Rectangle (Cr, 0.0, 0.0, Tape_Width,
                    Rate_Max * Rate_Step_Height + Size.Y);
         Fill (Cr);

         Set_Source_Rgb (Cr, 0.0, 0.0, 0.0);
         for Step in 0 .. Integer (abs Rate_Max) loop
            if Step mod 5 = 0 then
               Tick_Width :=  Size.X * 0.1;
               --  Negative values are black on white
               Set_Source_Rgb (Cr, 0.0, 0.0, 0.0);
               Draw_Right_Text (Cr, Step'Img,
                                (X => Tape_Width * 0.85 - Tick_Width * 1.2,
                                 Y => Gdouble (Step) * Rate_Step_Height),
                                Text_Size);
               --  Positive values are white on Black
               Set_Source_Rgb (Cr, 1.0, 1.0, 1.0);
               Draw_Right_Text (Cr, Step'Img,
                                (X => Tape_Width * 0.85 - Tick_Width * 1.2,
                                 Y => -Gdouble (Step) * Rate_Step_Height),
                                Text_Size);
            else
               Tick_Width :=  Size.X * 0.06;
            end if;

            Set_Line_Width (Cr, Tick_Width / 10.0);

            Set_Source_Rgb (Cr, 0.0, 0.0, 0.0);
            Move_To (Cr, Tape_Width * 0.85, Gdouble (Step) * Rate_Step_Height);
            Line_To (Cr, Tape_Width * 0.85 - Tick_Width,
                     Gdouble (Step) * Rate_Step_Height);
            Stroke (Cr);

            Set_Source_Rgb (Cr, 1.0, 1.0, 1.0);
            Move_To (Cr, Tape_Width * 0.85,
                     -Gdouble (Step) * Rate_Step_Height);
            Line_To (Cr, Tape_Width * 0.85 - Tick_Width,
                     -Gdouble (Step) * Rate_Step_Height);
            Stroke (Cr);

         end loop;

      end Draw_Rate_Tape;

      -------------------
      -- Draw_Alt_Tape --
      -------------------

      procedure Draw_Alt_Tape is
         Tick_Width : Gdouble;
         Text_Size : constant Gdouble := Size.X * 0.06;
      begin

         if Rate_Val < -Rate_Max then
            Rate_Val := -Rate_Max;
         elsif Rate_Val > Rate_Max then
            Rate_Val := Rate_Max;
         end if;

         Set_Source_Rgb (Cr, 0.0, 0.0, 0.0);
         for Step in 0 .. Integer (Alt_Max / 5.0) loop
            if Step mod 2 = 0 then
               Tick_Width :=  Size.X * 0.1;
               --  Positive values are white on Black
               Set_Source_Rgb (Cr, 1.0, 1.0, 1.0);
               Draw_Right_Text (Cr, Integer'Image (Step * 5),
                                (X => Tape_Width * 0.85 - Tick_Width * 1.2,
                                 Y => -Gdouble (Step) * Alt_Step_Height),
                                Text_Size);
            else
               Tick_Width :=  Size.X * 0.06;
            end if;

            Set_Line_Width (Cr, Tick_Width / 10.0);

            Set_Source_Rgb (Cr, 1.0, 1.0, 1.0);
            Move_To (Cr, Tape_Width * 0.85,
                     -Gdouble (Step) * Alt_Step_Height);
            Line_To (Cr, Tape_Width * 0.85 - Tick_Width,
                     -Gdouble (Step) * Alt_Step_Height);
            Stroke (Cr);

         end loop;

      end Draw_Alt_Tape;
   begin
      Self.Draw_Frame (Cr);

      --  Real inner Size is only known after Draw_Frame()
      Size         := Self.Inner_Frame_Size;
      Title_Height := Size.Y * 0.10;
      Text_Size    := Size.X * 0.07;
      Tape_Width   := Size.X * 0.435;

      Save (Cr);

      Translate (Cr, Self.Inner_Frame_Pos.X, Self.Inner_Frame_Pos.Y);
      Scale (Cr, Self.Scale, Self.Scale);

      --  Black background
      Set_Source_Rgb (Cr, 0.0, 0.0, 0.0);
      Rectangle (Cr, 0.0, 0.0, Size.X, Size.Y);
      Fill (Cr);

      --  Titles
      Set_Source_Rgb (Cr, 0.15, 0.15, 0.15);
      Rectangle (Cr, 0.0, 0.0, Size.X, Title_Height);
      Fill (Cr);

      --  RANGE / ALT
      Set_Source_Rgba (Cr, 1.0, 1.0, 1.0, 0.2);
      Draw_Centered_Text (Cr   => Cr,
                          Text => "RANGE",
                          Pos  => (Size.X / 4.0, Title_Height * 0.3),
                          Size => Text_Size);
      Set_Source_Rgba (Cr, 1.0, 1.0, 1.0, 1.0);
      Draw_Centered_Text (Cr   => Cr,
                          Text => "ALT",
                          Pos  => (Size.X / 4.0, Title_Height * 0.6),
                          Size => Text_Size);

      --  RANGE RATE / ALT RATE
      Save (Cr);
      Translate (Cr, Size.X / 2.0, 0.0);
      Set_Source_Rgba (Cr, 1.0, 1.0, 1.0, 0.2);
      Draw_Centered_Text (Cr   => Cr,
                          Text => "RANGE RATE",
                          Pos  => (Size.X / 4.0, Title_Height * 0.3),
                          Size => Text_Size);
      Set_Source_Rgba (Cr, 1.0, 1.0, 1.0, 1.0);
      Draw_Centered_Text (Cr   => Cr,
                          Text => "ALT RATE",
                          Pos  => (Size.X / 4.0, Title_Height * 0.6),
                          Size => Text_Size);
      Restore (Cr);

      --  From here, everything we draw will be under the titles
      Translate (Cr, 0.0, Title_Height);
      Size.Y := Size.Y - Title_Height;

      --  Rate tape (Right)
      Save (Cr);
      Translate (Cr, Size.X * 0.52, 0.0);
      Rectangle (Cr, 0.0, 0.0, Tape_Width, Size.Y);
      Clip (Cr);
      Translate (Cr, 0.0, Size.Y / 2.0 + Rate_Val * Rate_Step_Height);
      Draw_Rate_Tape;
      Restore (Cr);

      --  Alt tape (Right)
      Save (Cr);
      Translate (Cr, Size.X * 0.02, 0.0);
      Rectangle (Cr, 0.0, 0.0, Tape_Width, Size.Y);
      Clip (Cr);
      Translate (Cr, 0.0, Size.Y / 2.0 + (Alt_Val / 5.0) * Alt_Step_Height);
      Draw_Alt_Tape;
      Restore (Cr);

      --  Center while line
      Set_Source_Rgb (Cr, 1.0, 1.0, 1.0);
      Set_Line_Width (Cr, Size.Y * 0.01);
      Move_To (Cr, Size.X / 2.0, -Title_Height);
      Line_To (Cr, Size.X / 2.0, Size.Y * 0.45);
      Stroke (Cr);
      Move_To (Cr, Size.X / 2.0, Size.Y * 0.55);
      Line_To (Cr, Size.X / 2.0, Size.Y);
      Stroke (Cr);

      --  Cursors
      --  Altitude
      Move_To (Cr, Size.X / 2.0, Size.Y * 0.48);
      Line_To (Cr, Size.X / 2.0, Size.Y * 0.52);
      Line_To (Cr, Size.X / 2.0 - Size.Y * 0.04, Size.Y * 0.50);
      Close_Path (Cr);
      Fill (Cr);
      --  Rate
      Move_To (Cr, Size.X - Size.Y * 0.01, Size.Y * 0.48);
      Line_To (Cr, Size.X - Size.Y * 0.01, Size.Y * 0.52);
      Line_To (Cr, Size.X - Size.Y * 0.05, Size.Y * 0.50);
      Close_Path (Cr);
      Fill_Preserve (Cr);
      Set_Source_Rgb (Cr, 0.0, 0.0, 0.0);
      Set_Line_Width (Cr, 1.0);
      Stroke (Cr);
      Restore (Cr);
   end Draw;

end Panels.Altitude;
