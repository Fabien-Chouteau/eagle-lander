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

with Lander; use Lander;
with Cairo; use Cairo;
with Panels; use Panels;
with Geom; use Geom;

package Timeline is

   type Simulation_Time is new Natural;

   function Lenght return Simulation_Time;
   procedure Set_Now (Index : Simulation_Time);
   function Get_Situation (Index : Simulation_Time) return Lander_Situation;
   procedure Set_Situation (Situ : Lander_Situation);

   type Overview_Panel is new Panel with null record;

   procedure Init (Self : in out Overview_Panel;
                   Pos  : Vector2D;
                   Size : Vector2D);

   procedure Draw (Self : in out Overview_Panel;
                   Cr : Cairo_Context);
end Timeline;
