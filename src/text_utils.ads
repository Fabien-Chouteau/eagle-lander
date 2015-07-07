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
with Pango; use Pango;
with Pango.Layout; use Pango.Layout;
with Cairo; use Cairo;
with Pango.Enums;
with Geom; use Geom;
package Text_Utils is
   function LM_Font (Cr : Cairo_Context; Size : Gdouble;
                     Gravity : Pango.Enums.Gravity :=
                       Pango.Enums.Pango_Gravity_South) return Pango_Layout;
   procedure Draw_Centered_Text (Cr : Cairo_Context;
                                 Text : String;
                                 Pos : Vector2D;
                                 Size : Gdouble;
                                 Gravity : Pango.Enums.Gravity :=
                                   Pango.Enums.Pango_Gravity_South);
   procedure Draw_Right_Text (Cr : Cairo_Context;
                              Text : String;
                              Pos : Vector2D;
                              Size : Gdouble);

end Text_Utils;
