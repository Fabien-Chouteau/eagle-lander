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

with Glib; use Glib;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Numerics;

with System.Dim.Mks; use System.Dim.Mks;
with Physics; use Physics;

package Lander is

   --  For landing envolope, see:
   --  An Analysis and a Historical Review
   --  of the Apollo Program
   --  Lunar Module Touchdown Dynamics
   Safe_Landing_Vel : constant Speed_Vect := (Speed (1.2), Speed (2.1));

   Safe_Landing_Pitch : constant Angle := Ada.Numerics.Pi / 6.0;

   type Lander_Situation is record
      Pos     : Position;
      Vel     : Speed_Vect;
      Pitch   : Angle := 0.0;
      Pitch_R : Angular_Velocity := Angular_Velocity (0.0);

      DPS_Throttle       : Dimentionless := 0.0;
      Left_RCS_Throttle  : Dimentionless := 0.0;
      Right_RCS_Throttle : Dimentionless := 0.0;

      RCS_Propellent_Mass  : Mass  := 633.0 * kg;
      DPS_Propellent_Mass  : Mass  := 8_874.2 * kg;
   end record;

   procedure Lander_Phys_Step (Elapsed : Time; Situ : in out Lander_Situation);
   procedure Phys_Step (Elapsed : Time);
   procedure Reset;

   function Get_Situation return Lander_Situation;
   procedure Set_Situation (Situ : Lander_Situation);
   procedure Print_Situation;

   function Get_RCS_Propellent_Level return Gdouble;
   function Get_DPS_Propellent_Level return Gdouble;

   function Get_Thrust_To_Weight return Gdouble;

   procedure Set_DPS_Throttle (Throttle : Gdouble);
   procedure Set_Left_RCS_Throttle (Throttle : Gdouble);
   procedure Set_Right_RCS_Throttle (Throttle : Gdouble);

   type Result_Type is (Crash, Out_Of_Bounds, Sucess, Not_Done_Yet);
   type Ending_Situation is record
      Situ    : Lander_Situation;
      Result  : Result_Type := Not_Done_Yet;
      Message : Unbounded_String := Null_Unbounded_String;
      Points  : Natural;
   end record;

   procedure Reset_Ending;
   function Are_We_Done_Yet return Boolean;
   function How_did_it_end return Ending_Situation;

private

   Moon_Gravity         : constant Acceleration := Acceleration (1.6);
   Dry_Mass             : constant Mass := 7_706.4  * kg;

   DPS_Max_Thrust       : constant Force := 44_400.0 * N;
   DPS_Specific_Impulse : constant Specific_Impulse
     := Specific_Impulse (311.0);
   DPS_Prop_Mass_Init   : constant Mass := 8_874.2 * kg;

   RCS_Max_Thrust       : constant Force := 440.0 * N;
   RCS_Specific_Impulse : constant Specific_Impulse
     := Specific_Impulse (290.0);
   RCS_Prop_Mass_Init   : constant Mass := 633.0 * kg;

   RCS_Dist_From_COG : constant System.Dim.Mks.Length := 2.0 * m;

   Lander_Situ : Lander_Situation;
   Ending_Situ : Ending_Situation;

end Lander;
