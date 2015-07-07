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

with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Text_IO; use Ada.Text_IO;

package body Geom is

   package Value_Functions is new
     Ada.Numerics.Generic_Elementary_Functions (Gdouble);
   use Value_Functions;

   package Angle_Functions is new
     Ada.Numerics.Generic_Elementary_Functions (Angle);
   use Angle_Functions;

   -----------------
   -- To_Vector2D --
   -----------------

   function To_Vector2D (V1, V2 : Gdouble) return Vector2D is
   begin
      return (X => V1, Y => V2);
   end To_Vector2D;

   -----------------
   -- To_Vector2D --
   -----------------

   function To_Vector2D (V1, V2 : Vector2D) return Vector2D is
   begin
      return (X => V2.X - V1.X, Y => V2.Y - V2.Y);
   end To_Vector2D;

   ---------------
   -- Magnitude --
   ---------------

   function Magnitude (Vect : Vector2D) return Gdouble is
   begin
      return Sqrt (Vect.X**2 + Vect.Y**2);
   end Magnitude;

   ---------------
   -- Normalize --
   ---------------

   function Normalize (Vect : Vector2D) return Vector2D is
      Mag : constant Gdouble := Magnitude (Vect);
   begin
      if Mag = 0.0 then
         return (X => 0.0, Y => 0.0);
      else
         return (X => Vect.X / Mag, Y => Vect.Y / Mag);
      end if;
   end Normalize;

   ------------
   -- Rotate --
   ------------

   function Rotate (Vect : Vector2D; Angle_Rad : Angle) return Vector2D is
   begin
      return (X => Vect.X * Gdouble (Cos (Angle_Rad)) -
                Vect.Y * Gdouble (Sin (Angle_Rad)),
              Y => Vect.X * Gdouble (Sin (Angle_Rad)) +
                Vect.Y * Gdouble (Cos (Angle_Rad)));
   end Rotate;

   -------------------
   -- Angle_Of_Vect --
   -------------------

   function Angle_Of_Vect (Vect : Vector2D) return Gdouble is
   begin
      return Arctan (Vect.Y, Vect.X);
   exception
      when others =>
         Put_Line ("ArcTan Exception");
         return 0.0;
   end Angle_Of_Vect;

   -------------------
   -- Angle_Of_Vect --
   -------------------

   function Angle_Of_Vect (Vect : Vector2D) return Angle is
      Ret : constant Gdouble := Angle_Of_Vect (Vect);
   begin
      return Angle (Ret);
   end Angle_Of_Vect;

   ---------
   -- "+" --
   ---------

   function "+" (A : Vector2D; B : Vector2D) return Vector2D is
   begin
      return (X => A.X + B.X, Y => A.Y + B.Y);
   end "+";

   ---------
   -- "+" --
   ---------

   function "+" (A : Vector2D; B : Gdouble) return Vector2D is
   begin
      return (X => A.X + B, Y => A.Y + B);
   end "+";

   ---------
   -- "-" --
   ---------

   function "-" (A : Vector2D; B : Vector2D) return Vector2D is
   begin
      return (X => A.X - B.X, Y => A.Y - B.Y);
   end "-";

   ---------
   -- "-" --
   ---------

   function "-" (A : Vector2D) return Vector2D is
   begin
      return (X => -A.X, Y => -A.Y);
   end "-";

   ---------
   -- "-" --
   ---------

   function "-" (A : Vector2D; B : Gdouble) return Vector2D is
   begin
      return (X => A.X - B, Y => A.Y - B);
   end "-";

   ---------
   -- "*" --
   ---------

   function "*" (A : Vector2D; B : Gdouble) return Vector2D is
   begin
      return (X => A.X * B, Y => A.Y * B);
   end "*";

   ---------
   -- "*" --
   ---------

   function "*" (A : Gdouble; B : Vector2D) return Vector2D is
   begin
      return (X => B.X * A, Y => B.Y * A);
   end "*";

   ---------
   -- "/" --
   ---------

   function "/" (A : Vector2D; B : Gdouble) return Vector2D is
   begin
      return (X => A.X / B, Y => A.Y / B);
   end "/";

   ------------------
   -- Print_Vector --
   ------------------

   procedure Print_Vector (Vector : Vector2D; Prefix : String) is
   begin
      Put_Line (Prefix & "(X => " & Vector.X'Img
                  & ", Y =>" & Vector.Y'Img & ")");
   end Print_Vector;

   ---------------------
   -- Angle_Normalize --
   ---------------------

   function Angle_Normalize (A : Angle) return Angle is
      Ret : Angle := A;
   begin
      while Ret > Ada.Numerics.Pi loop
         Ret := Ret - Ada.Numerics.Pi * 2.0;
      end loop;

      while Ret < -Ada.Numerics.Pi loop
         Ret := Ret + Ada.Numerics.Pi * 2.0;
      end loop;
      return Ret;
   end Angle_Normalize;

   ----------------
   -- Angle_Diff --
   ----------------

   function Angle_Diff (A, B : Angle) return Angle is
   begin
      return Angle_Normalize (A - B);
   end Angle_Diff;

end Geom;
