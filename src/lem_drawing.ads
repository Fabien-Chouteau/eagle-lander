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
with Lander; use Lander;
with System.Dim.Mks; use System.Dim.Mks;

package LEM_Drawing is
   procedure Draw (Cr : Cairo_Context; Situ : Lander_Situation);
   procedure Draw_Forecast_And_Speed_Vect
     (Cr        : Cairo_Context;
      Situ      : Lander_Situation;
      Step      : Time;
      Iteration : Positive);

end LEM_Drawing;
