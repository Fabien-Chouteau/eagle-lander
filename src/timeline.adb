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

with Ada.Containers.Vectors;
with Ada.Containers;
with Glib; use Glib;
with Ada.Numerics;

package body Timeline is

   package Situation_Vecs is new Ada.Containers.Vectors
     (Element_Type => Lander_Situation,
      Index_Type   => Positive);

   Vect : Situation_Vecs.Vector;

   ------------
   -- Lenght --
   ------------

   function Lenght return Simulation_Time is
   begin
      return Simulation_Time (Vect.Length);
   end Lenght;

   -------------
   -- Set_Now --
   -------------

   procedure Set_Now (Index : Simulation_Time) is
   begin
      Vect.Set_Length (Ada.Containers.Count_Type (Index));
   end Set_Now;

   -------------------
   -- Get_Situation --
   -------------------

   function Get_Situation (Index : Simulation_Time) return Lander_Situation is
   begin
      return Vect (Positive (Index));
   end Get_Situation;

   -------------------
   -- Set_Situation --
   -------------------

   procedure Set_Situation (Situ : Lander_Situation) is
   begin
      Vect.Append (Situ);
   end Set_Situation;

   ----------
   -- Init --
   ----------

   procedure Init
     (Self : in out Overview_Panel;
      Pos  :        Vector2D;
      Size :        Vector2D)
   is
   begin
      Self.Pos  := Pos;
      Self.Size := Size;
   end Init;

   ----------
   -- Draw --
   ----------

   procedure Draw (Self : in out Overview_Panel; Cr : Cairo_Context) is
      X           : constant         := -2000.0;
      Y           : constant         := 0.0;
      Height      : constant         := 5000.0;
      Width       : constant         := 12000.0;
      Margin      : constant Gdouble := (Self.Size.X * 0.02) / Self.Scale;
      Screen_Size : Vector2D         := Self.Inner_Frame_Size - Margin * 2.0;
   begin
      Self.Draw_Frame (Cr);

      if Screen_Size.X <= 0.0 then
         Screen_Size.X := 1.0;
      end if;
      if Screen_Size.Y <= 0.0 then
         Screen_Size.Y := 1.0;
      end if;

      Save (Cr);
      Translate (Cr, Self.Inner_Frame_Pos.X, Self.Inner_Frame_Pos.Y);
      Scale (Cr, Self.Scale, Self.Scale);

      Scale (Cr, 1.0, -1.0);
      Translate (Cr, Margin, -Screen_Size.Y - Margin);
      Scale (Cr, Screen_Size.X / Width, Screen_Size.Y / Height);
      Translate (Cr, -X, -Y);

      --  white Screen
      Set_Source_Rgb (Cr, 1.0, 1.0, 1.0);
      Rectangle (Cr, X, Y, Width, Height);
      Fill_Preserve (Cr);

      --  Create A Mask
      Clip (Cr);

      Set_Line_Width (Cr, Height / 50.0);

      Set_Source_Rgb (Cr, 1.0, 0.0, 0.0);
      Arc (Cr, 0.0, 0.0, Height / 20.0, 0.0, 2.0 * Ada.Numerics.Pi);
      Stroke (Cr);
      Arc (Cr, 0.0, 0.0, Height / 40.0, 0.0, 2.0 * Ada.Numerics.Pi);
      Fill (Cr);

      Set_Source_Rgb (Cr, 0.0, 0.0, 1.0);
      Arc
        (Cr,
         Gdouble (Lander.Get_Situation.Pos.X),
         Gdouble (Lander.Get_Situation.Pos.Y),
         Height / 20.0,
         0.0,
         2.0 * Ada.Numerics.Pi);
      Set_Line_Cap (Cr, Cairo_Line_Cap_Round);
      Stroke (Cr);
      Arc
        (Cr,
         Gdouble (Lander.Get_Situation.Pos.X),
         Gdouble (Lander.Get_Situation.Pos.Y),
         Height / 40.0,
         0.0,
         2.0 * Ada.Numerics.Pi);
      Fill (Cr);

      for Situ of Vect loop
         Line_To (Cr, Gdouble (Situ.Pos.X), Gdouble (Situ.Pos.Y));
      end loop;
      Set_Source_Rgb (Cr, 0.0, 1.0, 0.0);
      Stroke (Cr);
      Restore (Cr);
   end Draw;

end Timeline;
