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

with System.Dim.Mks; use System.Dim.Mks;
with Glib; use Glib;

package Geom is

   type Vector2D is record
      X, Y : Gdouble;
   end record;

   Vector2D_Zero : constant Vector2D := (X => 0.0, Y => 0.0);

   function To_Vector2D (V1, V2 : Gdouble) return Vector2D;
   function To_Vector2D (V1, V2 : Vector2D) return Vector2D;

   function Magnitude (Vect : Vector2D) return Gdouble;
   function Normalize (Vect : Vector2D) return Vector2D;
   function Angle_Of_Vect (Vect : Vector2D) return Gdouble;
   function Angle_Of_Vect (Vect : Vector2D) return Angle;
   function Rotate (Vect : Vector2D; Angle_Rad : Angle) return Vector2D;

   function "+" (A : Vector2D; B : Vector2D) return Vector2D;
   function "+" (A : Vector2D; B : Gdouble) return Vector2D;
   function "-" (A : Vector2D; B : Vector2D) return Vector2D;
   function "-" (A : Vector2D) return Vector2D;
   function "-" (A : Vector2D; B : Gdouble) return Vector2D;
   function "*" (A : Vector2D; B : Gdouble) return Vector2D;
   function "*" (A : Gdouble; B : Vector2D) return Vector2D;
   function "/" (A : Vector2D; B : Gdouble) return Vector2D;

   procedure Print_Vector (Vector : Vector2D; Prefix : String);

   function Angle_Normalize (A : Angle) return Angle;
   function Angle_Diff (A, B : Angle) return Angle;

end Geom;
