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
with Ada.Numerics.Float_Random; use Ada.Numerics.Float_Random;
with Glib; use Glib;
with Geom; use Geom;
with Physics; use Physics;

package body LEM_Drawing is
   procedure Antena (Cr : Cairo_Context);
   procedure DCS (Cr : Cairo_Context);
   procedure Front_Leg (Cr : Cairo_Context);
   procedure Side_Leg (Cr : Cairo_Context);
   procedure Draw_Thrust (Cr     : Cairo_Context;
                          Pos    : Vector2D;
                          Width  : Gdouble;
                          Thrust : Gdouble);
   procedure Draw_LEM (Cr : Cairo_Context);

   G : Generator;

   ------------
   -- Antena --
   ------------

   procedure Antena (Cr : Cairo_Context) is
   begin
      Arc (Cr, 0.0, 0.0, 15.0, 0.0, Ada.Numerics.Pi * 2.0);
      Set_Source_Rgb (Cr, 0.5, 0.5, 0.5);
      Fill (Cr);
      Arc (Cr, 0.0, 0.0, 15.0, 0.0, Ada.Numerics.Pi * 2.0);
      Arc (Cr, 0.0, 0.0, 12.0, 0.0, Ada.Numerics.Pi * 2.0);
      Arc (Cr, 0.0, 0.0, 4.0, 0.0, Ada.Numerics.Pi * 2.0);
      Set_Source_Rgb (Cr, 0.0, 0.0, 0.0);
      Stroke (Cr);
   end Antena;

   ---------
   -- DCS --
   ---------

   procedure DCS (Cr : Cairo_Context) is
   begin
      for i in 1 .. 2 loop
         Move_To (Cr, 3.0, 0.0);
         Line_To (Cr, -6.0, -7.0);
         Line_To (Cr, -16.0, -7.0);
         Line_To (Cr, -18.0, 11.0);
         Line_To (Cr, -5.0, 11.0);
         Close_Path (Cr);
         if i = 1 then
            Set_Source_Rgb (Cr, 0.5, 0.5, 0.5);
            Fill (Cr);
         else
            Set_Source_Rgb (Cr, 0.0, 0.0, 0.0);
            Stroke (Cr);
         end if;
      end loop;
      Move_To (Cr, 5.0, 10.0);
      Line_To (Cr, 13.0, 63.0);
      Line_To (Cr, -1.0, 70.0);
      Line_To (Cr, -7.0, 10.0);
      Close_Path (Cr);
      Set_Source_Rgb (Cr, 0.0, 0.0, 0.0);
      Fill (Cr);
      Rotate (Cr, -Ada.Numerics.Pi / 2.0);
      for i in 1 .. 3 loop
         Save (Cr);
         Rotate (Cr, (Ada.Numerics.Pi / 2.0) * Gdouble (i));
         Scale (Cr, 0.3, 1.7);
         Arc (Cr, 0.0, -9.0, 10.0, 0.0, Ada.Numerics.Pi);
         Close_Path (Cr);
         Set_Source_Rgb (Cr, 0.8, 0.8, 0.8);
         Fill (Cr);
         Arc (Cr, 0.0, -9.0, 10.0, 0.0, Ada.Numerics.Pi);
         Close_Path (Cr);
         Set_Source_Rgb (Cr, 0.0, 0.0, 0.0);
         Stroke (Cr);
         Restore (Cr);
      end loop;
      Set_Source_Rgb (Cr, 0.0, 0.0, 0.0);
      Arc (Cr, 0.0, 0.0, 4.0, 0.0, Ada.Numerics.Pi * 2.0);
      Close_Path (Cr);
      Stroke (Cr);
      Arc (Cr, 0.0, 0.0, 4.0, 0.0, Ada.Numerics.Pi * 2.0);
      Close_Path (Cr);
      Fill (Cr);
   end DCS;

   ---------------
   -- Front_Leg --
   ---------------

   procedure Front_Leg (Cr : Cairo_Context) is
      procedure Half;
      procedure Half is
      begin
         for j in 1 .. 2 loop
            Move_To (Cr, 0.0, 17.0);
            Line_To (Cr, -31.0, 0.0);
            Line_To (Cr, -26.0, 0.0);
            Line_To (Cr, 0.0, 13.0);
            Close_Path (Cr);
            Move_To (Cr, -5.0, 27.0);
            Line_To (Cr, -22.0, 63.0);
            Line_To (Cr, -26.0, 63.0);
            Line_To (Cr, -5.0, 20.0);
            Close_Path (Cr);
            Move_To (Cr, -29.0, 57.0);
            Line_To (Cr, -37.0, 77.0);
            Line_To (Cr, -33.0, 77.0);
            Line_To (Cr, -25.0, 57.0);
            Close_Path (Cr);
            Move_To (Cr, -33.0, 77.0);
            Line_To (Cr, -5.0, 83.0);
            Line_To (Cr, -5.0, 80.0);
            Line_To (Cr, -32.0, 74.0);
            Close_Path (Cr);
            Move_To (Cr, -32.0, 74.0);
            Line_To (Cr, -5.0, 70.0);
            Line_To (Cr, -5.0, 68.0);
            Line_To (Cr, -31.0, 72.0);
            Close_Path (Cr);
            Move_To (Cr, 0.0, 16.0);
            Line_To (Cr, -5.0, 20.0);
            Line_To (Cr, -5.0, 90.0);
            Line_To (Cr, 0.0, 90.0);
            Close_Path (Cr);
            Move_To (Cr, 0.0, 90.0);
            Line_To (Cr, -2.5, 90.0);
            Line_To (Cr, -2.5, 116.0);
            Line_To (Cr, 0.0, 116.0);
            Close_Path (Cr);
            Move_To (Cr, 0.0, 116.0);
            Arc (Cr, 0.0, 101.0, 25.0, 0.0 + Ada.Numerics.Pi / 5.0,
                 Ada.Numerics.Pi - Ada.Numerics.Pi / 5.0);
            Close_Path (Cr);

            if j = 1 then
               Set_Source_Rgb (Cr, 0.8, 0.8, 0.0);
               Fill (Cr);
            else
               Set_Source_Rgb (Cr, 0.0, 0.0, 0.0);
               Stroke (Cr);
            end if;
         end loop;
         --  Ladder
         Save (Cr);
         Set_Source_Rgb (Cr, 0.0, 0.0, 0.0);
         Move_To (Cr, -8.0, 23.0);
         Line_To (Cr, -11.0, 23.0);
         Line_To (Cr, -11.0, 80.0);
         Line_To (Cr, -8.0, 80.0);
         Close_Path (Cr);
         Fill (Cr);
         for i in 1 .. 10 loop
            Move_To (Cr, 0.0, 25.0 + Gdouble (i) * 5.5);
            Line_To (Cr, -8.0, 25.0 + Gdouble (i) * 5.5);
            Line_To (Cr, -8.0, 27.0 + Gdouble (i) * 5.5);
            Line_To (Cr, 0.0, 27.0 + Gdouble (i) * 5.5);
            Fill (Cr);
         end loop;
         Move_To (Cr, 0.0, -9.0);
         Line_To (Cr, -20.0, -9.0);
         Line_To (Cr, -21.0, 12.0);
         Line_To (Cr, 0.0, 12.0);
         Fill (Cr);
         Set_Source_Rgb (Cr, 1.0, 1.0, 1.0);
         Set_Line_Width (Cr, 1.0);
         for i in 1 .. 5 loop
            Move_To (Cr, 0.0, -9.0 + Gdouble (i) * 4.3);
            Line_To (Cr, -19.0, -9.0 + Gdouble (i) * 4.3);
            Stroke (Cr);
         end loop;
         Restore (Cr);
      end Half;
   begin
      Half;
      Scale (Cr, -1.0, 1.0);
      Half;
   end Front_Leg;

   --------------
   -- Side_Leg --
   --------------

   procedure Side_Leg (Cr : Cairo_Context) is
   begin
      for i in 1 .. 2 loop
         Move_To (Cr, 0.0, 0.0);
         Line_To (Cr, -37.0, 13.0);
         Line_To (Cr, -37.0, 20.0);
         Line_To (Cr, 0.0, 7.0);
         Close_Path (Cr);
         Move_To (Cr, -35.0, 17.0);
         Line_To (Cr, -44.0, 29.0);
         Line_To (Cr, -68.0, 83.0);
         Line_To (Cr, -63.0, 86.0);
         Line_To (Cr, -63.0, 86.0);
         Line_To (Cr, -38.0, 29.0);
         Close_Path (Cr);
         Move_To (Cr, -37.0, 20.0);
         Line_To (Cr, 0.0, 68.0);
         Line_To (Cr, 0.0, 63.0);
         Line_To (Cr, -37.0, 15.0);
         Close_Path (Cr);
         Move_To (Cr, 0.0, 70.0);
         Line_To (Cr, -28.0, 72.0);
         Line_To (Cr, -28.0, 69.0);
         Line_To (Cr, 0.0, 67.0);
         Close_Path (Cr);
         Move_To (Cr, -28.0, 72.0);
         Line_To (Cr, -62.0, 84.0);
         Line_To (Cr, -61.0, 81.0);
         Line_To (Cr, -28.0, 70.0);
         Close_Path (Cr);
         Move_To (Cr, -22.0, 68.0);
         Line_To (Cr, 0.0, 52.0);
         Line_To (Cr, 0.0, 50.0);
         Line_To (Cr, -27.0, 69.0);
         Close_Path (Cr);
         Move_To (Cr, -64.0, 86.0);
         Line_To (Cr, -77.0, 112.0);
         Arc (Cr, -79.0, 97.0, 25.0, 0.0 + Ada.Numerics.Pi / 5.0,
              Ada.Numerics.Pi - Ada.Numerics.Pi / 5.0);
         Line_To (Cr, -81.0, 112.0);
         Line_To (Cr, -67.0, 84.0);
         Close_Path (Cr);
         if i = 1 then
            Set_Source_Rgb (Cr, 0.8, 0.8, 0.0);
            Fill (Cr);
         else
            Set_Source_Rgb (Cr, 0.0, 0.0, 0.0);
            Stroke (Cr);
         end if;
      end loop;
   end Side_Leg;

   --------------
   -- Draw_LEM --
   --------------

   procedure Draw_LEM (Cr : Cairo_Context) is
   begin
      --  center_x := width / 2.0;
      --  center_y := height / 2.0;

      Save (Cr);
      --  Translate (Cr, center_x + 5.0, center_y + 21.0);

      Set_Line_Width (Cr, 1.5);
      --  DPS
      Save (Cr);
      Scale (Cr, 0.25, 1.0);
      Arc (Cr, 0.0, 104.0, 100.0, Ada.Numerics.Pi, 0.0);
      Close_Path (Cr);
      Set_Source_Rgb (Cr, 0.8, 0.8, 0.8);
      Fill (Cr);
      Arc (Cr, 0.0, 104.0, 100.0, Ada.Numerics.Pi, 0.0);
      Close_Path (Cr);
      Set_Source_Rgb (Cr, 0.0, 0.0, 0.0);
      Stroke (Cr);
      Restore (Cr);

      --  Main frame
      for i in 1 .. 2 loop
         Line_To (Cr, -23.0, -10.0);
         Line_To (Cr, -27.0, -5.0);
         Line_To (Cr, -62.0, -5.0);
         Line_To (Cr, -60.0, -10.0);
         Line_To (Cr, -74.0, -30.0);
         Line_To (Cr, -54.0, -53.0);
         Line_To (Cr, -54.0, -105.0);
         Line_To (Cr, -45.0, -118.0);
         Line_To (Cr, 45.0, -118.0);
         Line_To (Cr, 55.0, -105.0);
         Line_To (Cr, 55.0, -53.0);
         Line_To (Cr, 65.0, -58.0);
         Line_To (Cr, 80.0, -58.0);
         Line_To (Cr, 102.0, -45.0);
         Line_To (Cr, 102.0, -17.0);
         Line_To (Cr, 90.0, -10.0);
         Line_To (Cr, 76.0, -6.0);
         Line_To (Cr, 46.0, -6.0);
         Line_To (Cr, 26.0, -12.0);
         Move_To (Cr, -54.0, -105.0);
         Line_To (Cr, -60.0, -105.0);
         Line_To (Cr, -60.0, -48.0);
         Line_To (Cr, -55.0, -54.0);
         Move_To (Cr, 54.0, -105.0);
         Line_To (Cr, 62.0, -105.0);
         Line_To (Cr, 62.0, -59.0);
         Line_To (Cr, 55.0, -55.0);
         if i = 1 then
            Set_Source_Rgba (Cr, 0.6, 0.6, 0.6, 1.0);
            Fill (Cr);
         else
            Set_Source_Rgb (Cr, 0.0, 0.0, 0.0);
            Stroke (Cr);
         end if;
      end loop;
      Set_Source_Rgb (Cr, 0.0, 0.0, 0.0);
      Move_To (Cr, -54.0, -105.0);
      Line_To (Cr, -60.0, -85.0);
      Line_To (Cr, -54.0, -65.0);
      Move_To (Cr, 89.0, -24.0);
      Line_To (Cr, 89.0, -10.0);
      Move_To (Cr, 89.0, -24.0);
      Line_To (Cr, 76.0, -6.0);
      Move_To (Cr, 89.0, -24.0);
      Line_To (Cr, 102.0, -17.0);
      Move_To (Cr, 89.0, -24.0);
      Line_To (Cr, 89.0, -39.0);
      Move_To (Cr, 89.0, -39.0);
      Line_To (Cr, 102.0, -45.0);
      Move_To (Cr, 89.0, -39.0);
      Line_To (Cr, 65.0, -58.0);
      Move_To (Cr, -52.0, -26.0);
      Line_To (Cr, -60.0, -10.0);
      Move_To (Cr, -52.0, -26.0);
      Line_To (Cr, -49.0, -5.0);
      Move_To (Cr, -52.0, -26.0);
      Line_To (Cr, -57.0, -32.0);
      Line_To (Cr, -73.0, -32.0);
      Move_To (Cr, -57.0, -32.0);
      Line_To (Cr, -55.0, -40.0);
      Stroke (Cr);
      Line_To (Cr, -54.0, -53.0);
      Line_To (Cr, -54.0, -105.0);
      Line_To (Cr, -45.0, -118.0);
      Line_To (Cr, 45.0, -118.0);
      Line_To (Cr, 55.0, -105.0);
      Line_To (Cr, 55.0, -53.0);
      Close_Path (Cr);
      Set_Source_Rgb (Cr, 0.2, 0.2, 0.2);
      Fill (Cr);

      --  cockpit
      Set_Source_Rgb (Cr, 0.0, 0.0, 0.0);
      Arc (Cr, 0.0, -55.0, 51.0, 0.0, 2.0 * Ada.Numerics.Pi);
      Stroke (Cr);
      Set_Source_Rgb (Cr, 0.0, 0.0, 0.0);
      Arc (Cr, 0.0, -55.0, 51.0, Ada.Numerics.Pi, 0.0);
      Close_Path (Cr);
      Fill (Cr);
      Set_Source_Rgb (Cr, 0.7, 0.7, 0.7);
      Arc (Cr, 0.0, -55.0, 51.0, -Ada.Numerics.Pi + Ada.Numerics.Pi / 5.2,
           -Ada.Numerics.Pi / 5.2);
      Line_To (Cr, 0.0, -50.0);
      Close_Path (Cr);
      Fill (Cr);

      --  Windows
      Set_Source_Rgb (Cr, 0.5, 0.5, 1.0);
      Line_To (Cr, -17.0, -64.0);
      Line_To (Cr, -42.0, -84.0);
      Line_To (Cr, -15.0, -90.0);
      Close_Path (Cr);
      Fill (Cr);
      Line_To (Cr, 17.0, -64.0);
      Line_To (Cr, 42.0, -84.0);
      Line_To (Cr, 15.0, -90.0);
      Close_Path (Cr);
      Fill (Cr);

      --  Hatch
      Set_Source_Rgb (Cr, 0.4, 0.4, 0.4);
      Line_To (Cr, 0.0, -8.0);
      Line_To (Cr, 20.0, -8.0);
      Line_To (Cr, 14.0, -57.0);
      Line_To (Cr, -14.0, -57.0);
      Line_To (Cr, -20.0, -8.0);
      Close_Path (Cr);
      Fill (Cr);
      for i in 1 .. 2 loop
         Line_To (Cr, 0.0, -10.0);
         Line_To (Cr, 10.0, -10.0);
         Line_To (Cr, 14.0, -15.0);
         Line_To (Cr, 14.0, -36.0);
         Line_To (Cr, 10.0, -42.0);
         Line_To (Cr, -10.0, -42.0);
         Line_To (Cr, -14.0, -36.0);
         Line_To (Cr, -14.0, -15.0);
         Line_To (Cr, -10.0, -10.0);
         Close_Path (Cr);
         if i = 1 then
            Set_Source_Rgb (Cr, 0.6, 0.6, 0.6);
            Fill (Cr);
         else
            Set_Source_Rgb (Cr, 0.0, 0.0, 0.0);
            Stroke (Cr);
         end if;
      end loop;
      Set_Source_Rgb (Cr, 0.0, 0.0, 0.0);
      Rectangle (Cr, -6.5, -42.0, 13.0, 32.0);
      Rectangle (Cr, -6.5, -34.0, 13.0, 16.0);
      Stroke (Cr);

      --  Center frame
      for i in 1 .. 2 loop
         Set_Source_Rgb (Cr, 0.0, 0.0, 0.0);
         Line_To (Cr, -23.0, -10.0);
         Line_To (Cr, -11.0, -117.0);
         Line_To (Cr, 11.0, -117.0);
         Line_To (Cr, 23.0, -10.0);
         Line_To (Cr, 20.0, -8.0);
         Line_To (Cr, 14.0, -57.0);
         Line_To (Cr, -14.0, -57.0);
         Line_To (Cr, -20.0, -8.0);
         Close_Path (Cr);
         if i = 1 then
            Set_Source_Rgb (Cr, 0.6, 0.6, 0.6);
            Fill (Cr);
         else
            Set_Source_Rgb (Cr, 0.0, 0.0, 0.0);
            Stroke (Cr);
         end if;
      end loop;
      Arc (Cr, 0.0, -70.0, 4.0, 0.0, 2.0 * Ada.Numerics.Pi);

      --  Legs
      Set_Source_Rgb (Cr, 0.4, 1.0, 1.0);
      Save (Cr);
      Translate (Cr, -85.0, 0.0);
      Side_Leg (Cr);
      Restore (Cr);
      Save (Cr);
      Translate (Cr, 85.0, 0.0);
      Scale (Cr, -1.0, 1.0);
      Side_Leg (Cr);
      Restore (Cr);

      --  Descent stage
      for i in 1 .. 2 loop
         Rectangle (Cr, -85.0, 00.0, 170.0, 70.0);
         Rectangle (Cr, -85.0, 00.0, 57.0, 70.0);
         Rectangle (Cr, -85.0, 00.0, 57.0, 70.0);
         Rectangle (Cr, 29.0, 00.0, 56.0, 70.0);
         Rectangle (Cr, -68.0, 70.0, 140.0, 8.0);
         if i = 1 then
            Set_Source_Rgb (Cr, 0.6, 0.6, 0.0);
            Fill (Cr);
         else
            Set_Source_Rgb (Cr, 0.0, 0.0, 0.0);
            Stroke (Cr);
         end if;
      end loop;

      Front_Leg (Cr);

      --  DCS
      Save (Cr);
      Translate (Cr, 69.0, -50.5);
      DCS (Cr);
      Restore (Cr);
      Save (Cr);
      Translate (Cr, -69.0, -50.5);
      Scale (Cr, -1.0, 1.0);
      DCS (Cr);
      Restore (Cr);

      Save (Cr);
      Translate (Cr, 0.0, -138.0);
      Rectangle (Cr, -17.0, 12.0, 34.0, 5.0);
      Rectangle (Cr, -13.0, 17.0, 5.0, 3.0);
      Rectangle (Cr, -3.0, 17.0, 5.0, 3.0);
      Rectangle (Cr, 9.0, 17.0, 5.0, 3.0);
      Set_Source_Rgb (Cr, 0.2, 0.2, 0.2);
      Fill (Cr);
      Antena (Cr);
      Restore (Cr);

      Save (Cr);
      Translate (Cr, 71.0, -132.0);
      Rectangle (Cr, -25.0, 13.0, 34.0, 5.0);
      Rectangle (Cr, -5.0, 23.0, 10.0, 5.0);
      Rectangle (Cr, -0.0, 18.0, 2.0, 5.0);
      Set_Source_Rgb (Cr, 0.2, 0.2, 0.2);
      Fill (Cr);
      Antena (Cr);
      Restore (Cr);

      Restore (Cr);
   end Draw_LEM;

   -----------------
   -- Draw_Thrust --
   -----------------

   procedure Draw_Thrust (Cr     : Cairo_Context;
                          Pos    : Vector2D;
                          Width  : Gdouble;
                          Thrust : Gdouble) is

      procedure Draw_Triangle (Depth : Gdouble);
      function Rand return Gdouble;

      -------------------
      -- Draw_Triangle --
      -------------------

      procedure Draw_Triangle (Depth : Gdouble) is
      begin
         Move_To (Cr, Pos.X, Pos.Y);
         Line_To (Cr, Pos.X - Width / 2.0, Pos.Y);
         Line_To (Cr, Pos.X, Pos.Y + Depth);
         Line_To (Cr, Pos.X + Width / 2.0, Pos.Y);
         Close_Path (Cr);
         Fill (Cr);
      end Draw_Triangle;

      ----------
      -- Rand --
      ----------

      function Rand return Gdouble is
      begin
         return ((Gdouble (Random (G)) - 0.5) * Thrust * 0.1);
      end Rand;
   begin
      Save (Cr);
      Set_Source_Rgb (Cr, 1.0, 0.0, 0.0);
      Draw_Triangle (-Thrust - Rand);
      Set_Source_Rgb (Cr, 1.0, 0.4, 0.0);
      Draw_Triangle (-Thrust * 0.5 - Rand);
      Restore (Cr);
   end Draw_Thrust;

   ----------
   -- Draw --
   ----------

   procedure Draw (Cr : Cairo_Context; Situ : Lander_Situation) is
   begin
      Save (Cr);
      Translate (Cr, Gdouble (Situ.Pos.X), Gdouble (Situ.Pos.Y));
      Rotate (Cr, Gdouble (Situ.Pitch));
      Scale (Cr, -0.03, 0.03);
      Rotate (Cr, Ada.Numerics.Pi);
      Rectangle (Cr, -5.0, 4.35, 10.0, 8.7);
      Set_Line_Width (Cr, 0.1);
      Stroke (Cr);

      if Situ.DPS_Propellent_Mass > 0.0 * kg then
         Draw_Thrust (Cr, (X => 0.0, Y => 100.0),
                      48.0,  Gdouble (-Situ.DPS_Throttle) * 150.0);
      end if;

      Draw_LEM (Cr);

      if Situ.RCS_Propellent_Mass > 0.0 * kg then
         if Situ.Right_RCS_Throttle > 0.0 then
            Draw_Thrust (Cr, (X => -69.0, Y => -65.5),
                         5.0, Gdouble (Situ.Right_RCS_Throttle) * 20.0);
         end if;
         if Situ.Right_RCS_Throttle < 0.0 then
            Draw_Thrust (Cr, (X => -69.0, Y => -35.0),
                         5.0, Gdouble (Situ.Right_RCS_Throttle) * 20.0);
         end if;
         if Situ.Left_RCS_Throttle > 0.0 then
            Draw_Thrust (Cr, (X => 69.0, Y => -65.5),
                         5.0, Gdouble (Situ.Left_RCS_Throttle) * 20.0);
         end if;
         if Situ.Left_RCS_Throttle < 0.0 then
            Draw_Thrust (Cr, (X => 69.0, Y => -35.0),
                         5.0, Gdouble (Situ.Left_RCS_Throttle) * 20.0);
         end if;
      end if;
      Restore (Cr);
   end Draw;

   ----------------------------------
   -- Draw_Forecast_And_Speed_Vect --
   ----------------------------------

   procedure Draw_Forecast_And_Speed_Vect
     (Cr        : Cairo_Context;
      Situ      : Lander_Situation;
      Step      : Time;
      Iteration : Positive)
   is

      F_Situ : Lander_Situation := Situ;

      --  Lander direction
      L_Vect_1 : constant Position :=
        F_Situ.Pos + Rotate ((0.0 * m, 20.0 * m), F_Situ.Pitch);
      L_Vect_2 : constant Position :=
        F_Situ.Pos + Rotate ((0.0 * m, -20.0 * m), F_Situ.Pitch);

      --  Lander speed vector direction
      V_Angle  : Angle;
      V_Vect_1, V_Vect_2 : Position;
   begin
      Save (Cr);

      Set_Line_Width (Cr, 0.2);
      Set_Source_Rgba (Cr, 1.0, 0.0, 0.0, 0.5);
      Move_To (Cr, Gdouble (L_Vect_1.X), Gdouble (L_Vect_1.Y));
      Line_To (Cr, Gdouble (L_Vect_2.X), Gdouble (L_Vect_2.Y));
      Stroke (Cr);

      if F_Situ.Vel /= (Speed (0.0), Speed (0.0)) then
         --  Lander speed vector direction
         V_Angle :=
           Angle_Of_Vect ((Gdouble (F_Situ.Vel.X), Gdouble (F_Situ.Vel.Y)));
         V_Vect_1 := F_Situ.Pos;
         V_Vect_2 := F_Situ.Pos + Rotate ((20.0 * m, 0.0 * m), V_Angle);

         Set_Source_Rgba (Cr, 0.0, 1.0, 0.0, 0.5);
         Move_To (Cr, Gdouble (V_Vect_1.X), Gdouble (V_Vect_1.Y));
         Line_To (Cr, Gdouble (V_Vect_2.X), Gdouble (V_Vect_2.Y));
         Stroke (Cr);
      end if;

      Set_Source_Rgba (Cr, 0.0, 0.0, 1.0, 0.5);
      Set_Line_Cap (Cr, Cairo_Line_Cap_Round);
      Set_Line_Width (Cr, 2.0);

      for N in 1 .. Iteration loop
         Lander_Phys_Step (Step, F_Situ);
         Move_To (Cr, Gdouble (F_Situ.Pos.X), Gdouble (F_Situ.Pos.Y));
         Line_To (Cr, Gdouble (F_Situ.Pos.X), Gdouble (F_Situ.Pos.Y));
         Stroke (Cr);

      end loop;
      Restore (Cr);
   end Draw_Forecast_And_Speed_Vect;

begin
   Reset (G, 42);
end LEM_Drawing;
