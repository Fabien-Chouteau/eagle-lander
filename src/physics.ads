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
with Ada.Numerics.Generic_Elementary_Functions;

package Physics is

   Simulation_Frequency : constant := 20;

   package Angle_Functions is new
     Ada.Numerics.Generic_Elementary_Functions (Angle);
   use Angle_Functions;

   subtype Acceleration is Mks_Type
     with
      Dimension => (
        Symbol => "m/s2",
        Meter  =>  1,
        Second => -2,
        others => 0);

   subtype Angular_Velocity is Mks_Type
     with
      Dimension => (
        Symbol => "rad/s",
        Second => -1,
        others => 0);

   subtype Anglular_Acceleration is Mks_Type
     with
      Dimension => (
        Symbol => "rad/s2",
        Second => -2,
        others => 0);

   subtype Torque is Mks_Type
     with
      Dimension => (
        Symbol => "Nm",
        Meter    => 2,
        Kilogram => 1,
        Second   => -2,
        others   => 0);

   subtype Moment_Of_Inertia is Mks_Type
     with
      Dimension => (
        Symbol => "kg.m2.rad-2",
        Meter    => 2,
        Kilogram => 1,
        others   => 0);

   subtype Specific_Impulse is Mks_Type
     with
      Dimension => (
        Symbol => "Ns/kg",
        Meter    => 1,
        Kilogram => 0,
        Second   => -1,
        others   => 0);

   subtype Dimentionless is Mks_Type;

   type Force_Vect is record
      X, Y : Force := Force (0.0);
   end record;

   type Acc_Vect is record
      X, Y : Acceleration := Acceleration (0.0);
   end record;

   type Speed_Vect is record
      X, Y : Speed := Speed (0.0);
   end record;

   type Position is record
      X, Y : Length := 0.0 * m;
   end record;

   function Rotate (Vect : Force_Vect; A : Angle) return Force_Vect is
      (X => Vect.X * Mks_Type (Cos (A)) - Vect.Y * Mks_Type (Sin (A)),
       Y => Vect.X * Mks_Type (Sin (A)) + Vect.Y * Mks_Type (Cos (A)));

   function Rotate (Vect : Position; A : Angle) return Position is
      (X => Vect.X * Mks_Type (Cos (A)) - Vect.Y * Mks_Type (Sin (A)),
       Y => Vect.X * Mks_Type (Sin (A)) + Vect.Y * Mks_Type (Cos (A)));

   function "+" (A, B : Force_Vect) return Force_Vect is
     (A.X + B.X, A.Y + B.Y);

   function "+" (A, B : Speed_Vect) return Speed_Vect is
     (A.X + B.X, A.Y + B.Y);

   function "+" (A, B : Position) return Position is
     (A.X + B.X, A.Y + B.Y);

   function "*" (A : Acc_Vect; B : Time) return Speed_Vect is
      (A.X * B, A.Y * B);

   function "*" (A : Speed_Vect; B : Time) return Position is
      (A.X * B, A.Y * B);

   function "/" (A : Force_Vect; B : Mass) return Acc_Vect is
     (A.X / B, A.Y / B);
end Physics;
