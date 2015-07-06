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

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Float_Random; use Ada.Numerics.Float_Random;
with Timeline;
with LEM_Drawing;
with Geom; use Geom;
with System.Dim.Mks_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Lander is

   package Float_IO is new Ada.Text_IO.Float_IO(Gdouble);

   G : Generator;

   procedure Check_Ending;
   procedure Lander_Phys_Step (Elapsed : Time; Situ : in out Lander_Situation);
   function Total_Mass (Situ : Lander_Situation) return Mass;
   function Weight (Situ : Lander_Situation) return Force_Vect;
   function Pitch_Moment_Of_Inertia (Siti : Lander_Situation)
                                     return Moment_Of_Inertia;
   function DPS_Thrust (Elapsed : Time; Situ : in out Lander_Situation)
                        return Force_Vect;
   procedure RCS_Thrust (Elapsed : Time;
                         Fsum : in out Force_Vect;
                         Pitch_F : in out Torque;
                         Situ : in out Lander_Situation);

   function Total_Mass (Situ : Lander_Situation) return Mass is
   begin
      return Dry_Mass + Situ.RCS_Propellent_Mass + Situ.DPS_Propellent_Mass;
   end Total_Mass;

   function Weight (Situ : Lander_Situation) return Force_Vect is
   begin
      return (X => Force (0.0), Y => -(Total_Mass (Situ) * Moon_Gravity));
   end Weight;

   function Pitch_Moment_Of_Inertia (Siti : Lander_Situation)
                                     return Moment_Of_Inertia
   is
   begin
      --  TODO: Compute real moment of inertia
      --  Value from Apollo 14 : https://www.hq.nasa.gov/alsj/a14/a14mr-a.htm
      return Moment_Of_Inertia (12_750.0);
   end Pitch_Moment_Of_Inertia;


   procedure RCS_Thrust (Elapsed : Time;
                         Fsum    : in out Force_Vect;
                         Pitch_F : in out Torque;
                         Situ    : in out Lander_Situation)
   is
      --  RCS thrust is multiplied by two because there are two RCS on each
      --  sides of the LM.
      L_Thrust : constant Force :=
        RCS_Max_Thrust * Situ.Left_RCS_Throttle * 2.0;
      R_Thrust : constant Force :=
        RCS_Max_Thrust * Situ.Right_RCS_Throttle * 2.0;
   begin
      if Situ.RCS_Propellent_Mass > Mass (0.0) then
         Situ.RCS_Propellent_Mass := Situ.RCS_Propellent_Mass
           - ((abs L_Thrust + abs R_Thrust)
              / (RCS_Specific_Impulse * 9.80665)) * Elapsed;

         Pitch_F := Pitch_F + (R_Thrust - L_Thrust) * RCS_Dist_From_COG;
         Fsum := Fsum +
           Rotate ((X => 0.0 * N, Y => R_Thrust + L_Thrust), Situ.Pitch);

         if Situ.RCS_Propellent_Mass < Mass (0.0) then
            Situ.RCS_Propellent_Mass := 0.0 * Kg;
         end if;
      end if;
   end RCS_Thrust;

   function DPS_Thrust (Elapsed : Time;
                        Situ    : in out Lander_Situation)
                        return Force_Vect
   is
      Thrust : constant Force := DPS_Max_Thrust * Situ.DPS_Throttle;
   begin
      if Situ.DPS_Propellent_Mass > Mass (0.0) then
         Situ.DPS_Propellent_Mass := Situ.DPS_Propellent_Mass
           - (Thrust / (DPS_Specific_Impulse * 9.80665)) * Elapsed;

         if Situ.DPS_Propellent_Mass < Mass (0.0) then
            Situ.DPS_Propellent_Mass := 0.0 * kg;
         end if;
         return Rotate ((X => 0.0 * N, Y => Thrust), Situ.Pitch);
      else
         return (0.0 * N, 0.0 * N);
      end if;
   end DPS_Thrust;

   procedure Lander_Phys_Step (Elapsed : Time; Situ : in out Lander_Situation)
   is
      FSum    : Force_Vect := (0.0 * N, 0.0 * N);
      Pitch_T : Torque  := Torque (0.0);
   begin

      FSum := FSum + Weight (Situ);

      FSum := FSum + DPS_Thrust (Elapsed, Situ);

      RCS_Thrust (Elapsed, FSum, Pitch_T, Situ);

      Situ.Vel := Situ.Vel + (FSum / Total_Mass (Situ)) * Elapsed;
      Situ.Pos := Situ.Pos + Situ.Vel * Time (Elapsed);

      Situ.Pitch_R := Situ.Pitch_R + (Pitch_T / Pitch_Moment_Of_Inertia (Situ)) * Elapsed;
      Situ.Pitch   := Angle_Normalize (Situ.Pitch + Situ.Pitch_R * Elapsed);
   end Lander_Phys_Step;

   procedure Phys_Step (Elapsed : Time) is
   begin
      if not Are_We_Done_Yet then
         Lander_Phys_Step (Elapsed, Lander_Situ);
         Timeline.Set_Situation (Lander_Situ);
         Check_Ending;
      end if;
   end Phys_Step;

   procedure Draw_Lander (Cr : Cairo_Context; Situ : Lander_Situation);
   procedure Draw_Thrust (Cr     : Cairo_Context;
                          Pos    : Vector2D;
                          Width  : Gdouble;
                          Thrust : Gdouble);

   procedure Draw_Thrust (Cr     : Cairo_Context;
                          Pos    : Vector2D;
                          Width  : Gdouble;
                          Thrust : Gdouble) is

      procedure Draw_Triangle (Depth : Gdouble);
      function Rand return Gdouble;

      procedure Draw_Triangle (Depth : Gdouble) is
      begin
         Move_To (Cr, Pos.X, Pos.Y);
         Line_To (Cr, Pos.X - Width / 2.0, Pos.Y);
         Line_To (Cr, Pos.X, Pos.Y + Depth);
         Line_To (Cr, Pos.X + Width / 2.0, Pos.Y);
         Close_Path (Cr);
         Fill (Cr);
      end Draw_Triangle;

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

   procedure Draw_Lander (Cr : Cairo_Context; Situ : Lander_Situation) is
   begin
      Save (Cr);
      Translate (Cr, Gdouble (Lander_Situ.Pos.X),
                 Gdouble (Lander_Situ.Pos.Y));
      Rotate (Cr, Gdouble (Lander_Situ.Pitch));
      Scale (Cr, -0.03, 0.03);
      Rotate (Cr, Ada.Numerics.Pi);
      Rectangle (Cr, -5.0, 4.35, 10.0, 8.7);
      Set_Line_Width (Cr, 0.1);
      Stroke (Cr);

      if Situ.DPS_Propellent_Mass > 0.0 * kg then
         Draw_Thrust (Cr, (X => 0.0, Y => 100.0),
                      48.0,  Gdouble (-Situ.DPS_Throttle) * 150.0);
      end if;

      LEM_Drawing.Draw (Cr);

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


   end Draw_Lander;

   procedure Draw (Cr : Cairo_Context) is
   begin
      Draw_Lander (Cr, Lander_Situ);
   end Draw;

   procedure Draw_Forecast_And_Speed_Vect (Cr : Cairo_Context; Step : Time;
                                           Iteration : Positive) is

      F_Situ : Lander_Situation := Lander_Situ;

      --  Lander direction
      L_Vect_1 : Position :=
        F_Situ.Pos + Rotate ((0.0 * m, 20.0 * m), F_Situ.Pitch);
      L_Vect_2 : Position :=
        F_Situ.Pos + Rotate ((0.0 * m, -20.0 * m), F_Situ.Pitch);

      -- Lander speed vector direction
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
         -- Lander speed vector direction
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

   procedure Reset is
   begin
      Reset_Ending;
      Lander_Situ.Pos := (X => 0.0 * m, Y => 50.0 * m);
      Lander_Situ.Vel := (X => Speed (0.0), Y => Speed (0.0));
      Lander_Situ.Pitch := 0.0 * rad;
      Lander_Situ.Pitch_R := Angular_Velocity (0.0);
      Lander_Situ.DPS_Throttle := 0.0;
      Lander_Situ.Left_RCS_Throttle := 0.0;
      Lander_Situ.Right_RCS_Throttle := 0.0;
      Lander_Situ.DPS_Propellent_Mass := DPS_Prop_Mass_Init;
      Lander_Situ.RCS_Propellent_Mass := RCS_Prop_Mass_Init;

      --  High Gate Apollo 11
      --  Based on "Apollo lunar descent and ascent trajectories"
      --  NASA TM X-58040, March 1970
      Lander_Situ.Pos := (X => 8334.0 * m, Y => 2290.0 * m);
      Lander_Situ.Vel := (X => Speed (-154.229), Y => Speed (-44.196));
      Lander_Situ.Pitch := -45.0 * (Ada.Numerics.Pi / 180.0);
      Lander_Situ.DPS_Propellent_Mass := DPS_Prop_Mass_Init / 5.0;
   end Reset;

   function Get_Situation return Lander_Situation is
   begin
      return Lander_Situ;
   end Get_Situation;

   procedure Set_Situation (Situ : Lander_Situation) is
   begin
      Lander_Situ := Situ;
   end Set_Situation;

   procedure Set_DPS_Throttle (Throttle : Gdouble) is
   begin
      --  DPS cannot be throttled between 0% and 10% as well as 60% and 100%
      if Throttle < 0.1 then
         Lander_Situ.DPS_Throttle := 0.0;
      elsif Throttle > 0.6 and then Throttle < 1.0 then
         if Lander_Situ.DPS_Throttle <= 0.6 then
            Lander_Situ.DPS_Throttle := 1.0;
         else
            Lander_Situ.DPS_Throttle := 0.6;
         end if;
      else
         Lander_Situ.DPS_Throttle := Dimentionless (Throttle);
      end if;
   end Set_DPS_Throttle;

   procedure Set_Left_RCS_Throttle (Throttle : Gdouble) is
   begin
      if Throttle < 0.1 and then Throttle > -0.1 then
         Lander_Situ.Left_RCS_Throttle := 0.0;
      elsif Throttle > 0.9 then
         Lander_Situ.Left_RCS_Throttle := 1.0;
      elsif Throttle < -0.9 then
         Lander_Situ.Left_RCS_Throttle := -1.0;
      else
         Lander_Situ.Left_RCS_Throttle := Dimentionless (Throttle);
      end if;
   end Set_Left_RCS_Throttle;

   procedure Set_Right_RCS_Throttle (Throttle : Gdouble) is
   begin
      if Throttle < 0.1 and then Throttle > -0.1 then
         Lander_Situ.Right_RCS_Throttle := 0.0;
      elsif Throttle >= 0.9 then
         Lander_Situ.Right_RCS_Throttle := 1.0;
      elsif Throttle <= -0.9 then
         Lander_Situ.Right_RCS_Throttle := -1.0;
      else
         Lander_Situ.Right_RCS_Throttle := Dimentionless (Throttle);
      end if;
   end Set_Right_RCS_Throttle;

   function Get_RCS_Propellent_Level return Gdouble is
   begin
      return Gdouble (Lander_Situ.RCS_Propellent_Mass / RCS_Prop_Mass_Init);
   end Get_RCS_Propellent_Level;

   function Get_DPS_Propellent_Level return Gdouble is
   begin
      return Gdouble (Lander_Situ.DPS_Propellent_Mass / DPS_Prop_Mass_Init);
   end Get_DPS_Propellent_Level;

   function Get_Thrust_To_Weight return Gdouble is
      Thrust : constant Force := DPS_Max_Thrust * Lander_Situ.DPS_Throttle;
   begin
      if Lander_Situ.DPS_Propellent_Mass = 0.0 * kg then
         return 0.0;
      else
         return Gdouble (Thrust / Weight (Lander_Situ).Y);
      end if;
   end Get_Thrust_To_Weight;

   procedure Print_Situation is
   begin
      Put_Line ("=== Lander Situation");
      Put_Line ("Pos X =>" & Lander_Situ.Pos.X'Img &
                  ", Y =>" & Lander_Situ.Pos.Y'Img);
      Put_Line ("Vel X =>" & Lander_Situ.Vel.X'Img &
                  ", Y =>" & Lander_Situ.Vel.Y'Img);
      Put_Line ("Pitch =>" & Lander_Situ.Pitch'Img);
      Put_Line ("Pitch_Rate =>" & Lander_Situ.Pitch_R'Img);
      Put_Line ("DPS_Throttle =>" & Lander_Situ.DPS_Throttle'Img);
      Put_Line ("DPS_Propellent_Mass =>"
                & Lander_Situ.DPS_Propellent_Mass'Img);
      Put_Line ("Left_RCS_Throttle =>" & Lander_Situ.Left_RCS_Throttle'Img);
      Put_Line ("Right_RCS_Throttle =>" & Lander_Situ.Right_RCS_Throttle'Img);
      Put_Line ("RCS_Propellent_Mass =>"
                & Lander_Situ.RCS_Propellent_Mass'Img);
   end Print_Situation;

   procedure Check_Ending is
      Left_Skid  : constant Position := Lander_Situ.Pos
        + Rotate ((-5.5 * m, -3.5 * m), Lander_Situ.Pitch);
      Right_Skid : constant Position := Lander_Situ.Pos
        + Rotate ((5.5 * m, -3.5 * m), Lander_Situ.Pitch);
      Right_top : constant Position := Lander_Situ.Pos
        + Rotate ((3.0 * m, 3.0 * m), Lander_Situ.Pitch);
      Left_Top : constant Position := Lander_Situ.Pos
        + Rotate ((-5.0 * m, 3.0 * m), Lander_Situ.Pitch);
      Str : String (1 .. 7);
   begin

      if abs Lander_Situ.Pitch_R > 3.0 * rad / s then
         Float_IO.Put (Str, Gdouble (Lander_Situ.Pitch_R), 2, 0);
         Set_Unbounded_String (Ending_Situ.Message,
           "Pitch rate too high: "
           & Str & " rad/s" & ASCII.CR & ASCII.LF);
         Ending_Situ.Result := Crash;
         Ending_Situ.Points := 0;
      end if;

      --  Check if something is touching ground
      if Left_Skid.Y <= 0.0 * m or else Right_Skid.Y <= 0.0  * m or else
        Left_Top.Y <= 0.0  * m or else Right_Top.Y <= 0.0  * m
      then

         Set_Unbounded_String (Ending_Situ.Message, "");

         if abs Lander_Situ.Vel.X > Safe_Landing_Vel.X then
            Float_IO.Put (Str, Gdouble (Lander_Situ.Vel.X), 2, 0);
            Append (Ending_Situ.Message,
                    "Horizontal velocity too high: "
                    & Str & " m/s" & ASCII.CR & ASCII.LF);
            Ending_Situ.Result := Crash;
            Ending_Situ.Points := 0;
         end if;
         if abs Lander_Situ.Vel.Y > Safe_Landing_Vel.Y then
            Float_IO.Put (Str, Gdouble (Lander_Situ.Vel.Y), 2, 0);
            Append (Ending_Situ.Message,
                    "Descent rate too high: "
                    & Str & " m/s" & ASCII.CR & ASCII.LF);
            Ending_Situ.Result := Crash;
            Ending_Situ.Points := 0;
         end if;
         if abs Lander_Situ.Pitch_R > Ada.Numerics.Pi / 6.0 * rad / s then
            Float_IO.Put (Str, Gdouble (Lander_Situ.Pitch_R), 2, 0);
            Append (Ending_Situ.Message,
                    "Pitch rate too high: "
                    & Str & " rad/s" & ASCII.CR & ASCII.LF);
            Ending_Situ.Result := Crash;
            Ending_Situ.Points := 0;
         end if;
         if abs Lander_Situ.Pitch > Ada.Numerics.Pi / 12.0 * rad then
            Float_IO.Put (Str, Gdouble (Lander_Situ.Pitch), 2, 0);
            Append (Ending_Situ.Message,
                      "Pitch too high: " & Str & " rad" & ASCII.CR & ASCII.LF);
            Ending_Situ.Result := Crash;
            Ending_Situ.Points := 0;
         end if;

         if Ending_Situ.Result /= Crash then
            Set_Unbounded_String (Ending_Situ.Message, "The Eagle has landed");
            Ending_Situ.Result := Sucess;
         end if;

         Ending_Situ.Situ := Lander_Situ;
      end if;


      if Lander_Situ.Pos.X > 10_000.0 * m
        or else Lander_Situ.Pos.X < -2_000.0 * m
        or else Lander_Situ.Pos.Y > 5_000.0 * m
        or else Lander_Situ.Pos.Y < -2_000.0 * m
      then
         Set_Unbounded_String (Ending_Situ.Message, "Out of simulation bounds");
         Ending_Situ.Result := Out_Of_Bounds;
         Ending_Situ.Situ := Lander_Situ;
         Ending_Situ.Points := 0;
      end if;
   end Check_Ending;

   function Are_We_Done_Yet return Boolean is
   begin
      return Ending_Situ.Result /= Not_Done_Yet;
   end Are_We_Done_Yet;

   procedure Reset_Ending is
   begin
      Ending_Situ.Result := Not_Done_Yet;
   end Reset_Ending;

   function How_did_it_end return Ending_Situation is
   begin
      return Ending_Situ;
   end How_did_it_end;

begin
   Reset (G, 42);
end Lander;
