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

with Cairo; use Cairo;

with Glib; use Glib;
with Ada.Numerics.Float_Random;
use Ada.Numerics.Float_Random;

package body Background is

   X : constant := -2000.0;
   Y : constant := -2000.0;
   Height : constant := 6000.0;
   Width : constant  := 12000.0;

   type Star is record
      X, Y : Gdouble;
      Size : Gdouble;
   end record;

   Stars : array (1 .. 15000) of Star;

   procedure Draw_Stars (Cr : Cairo_Context);
   procedure Draw_Ground (Cr : Cairo_Context);

   ----------------
   -- Draw_Stars --
   ----------------

   procedure Draw_Stars (Cr : Cairo_Context) is
   begin
      Save (Cr);
      Set_Source_Rgb (Cr, 1.0, 1.0, 1.0);
      Set_Line_Cap (Cr, Cairo_Line_Cap_Round);
      for S of Stars loop
         Set_Line_Width (Cr, S.Size);
         Move_To (Cr, S.X, S.Y);
         Line_To (Cr, S.X, S.Y);
         Stroke (Cr);
      end loop;
      Restore (Cr);
   end Draw_Stars;

   -----------------
   -- Draw_Ground --
   -----------------

   procedure Draw_Ground (Cr : Cairo_Context) is
   begin
      Save (Cr);
      Set_Source_Rgb (Cr, 0.5, 0.5, 0.5);
      Rectangle (Cr, X, 0.0, Width, Y);
      Fill (Cr);
      Restore (Cr);
   end Draw_Ground;

   ----------
   -- Draw --
   ----------

   procedure Draw (Cr : Cairo_Context) is
   begin
      Save (Cr);
      Set_Source_Rgb (Cr, 0.0, 0.0, 0.0);
      Paint (Cr);
      Restore (Cr);

      Draw_Stars (Cr);
      Draw_Ground (Cr);

      --  Draw Target
      for Cnt in 1 .. 5 loop
         Set_Source_Rgb (Cr, 1.0, 0.0, 0.0);
         Set_Line_Width (Cr, 0.1);
         Arc (Cr, 0.0, 0.0, (Height * 0.001) * Gdouble (Cnt), 0.0,
              2.0 * Ada.Numerics.Pi);
         Stroke (Cr);
      end loop;
   end Draw;

   G : Ada.Numerics.Float_Random.Generator;

begin
   Reset (G, 42);
   for S of Stars loop
      S.X := Gdouble (Random (G) * Width) + X;
      S.Y := Gdouble (Random (G)) * Height;
      S.Size := Gdouble (Random (G)) * 1.5;
   end loop;

end Background;
