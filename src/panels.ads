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
--    Foobar is distributed in the hope that it will be useful,              --
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
with Geom; use Geom;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Physics; use Physics;

package Panels is

   type Angle_Array is array (1 .. 4) of Gdouble;
   type Panel is abstract tagged record
      Pos               : Vector2D;
      Size              : Vector2D;
      Scale             : Gdouble := 1.0;
      Clicked           : Boolean;
      Resized           : Boolean;
      Click_Pos         : Vector2D;
      Frame_Initialized : Boolean;
      Screws_Angle      : Angle_Array;
      Inner_Frame_Pos   : Vector2D;
      Inner_Frame_Size  : Vector2D;
   end record;

   type Panel_Access is not null access all Panel'Class;

   function On_Click (P : in out Panel; Evt : Vector2D) return Boolean;
   function On_Resize (P : in out Panel; Evt : Vector2D) return Boolean;
   procedure On_Released (P : in out Panel; Evt : Vector2D);
   procedure On_Motion (P : in out Panel; Evt : Vector2D);

   procedure Draw_Frame (Self : in out Panel; Cr : Cairo_Context);

   procedure Set_Background_Color (Cr : Cairo_Context);

   type Gauge is new Panel with record
      Text : String_Access := new String'("N/A");
   end record;

   procedure Init (Self : in out Gauge;
                   Text : String;
                   Pos  : Vector2D;
                   Size : Vector2D);

   procedure Draw (Self : in out Gauge;
                   Cr : Cairo_Context;
                   Value : Gdouble);

   type Xpointer is new Panel with null record;

   procedure Init (Self : in out Xpointer;
                   Pos   : Vector2D;
                   Size : Gdouble);

   procedure Draw (Self  : in out Xpointer;
                   Cr    : Cairo_Context;
                   Value : Speed_Vect);

end Panels;
