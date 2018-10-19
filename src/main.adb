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
with GUI; use GUI;
with Gtk;
with Gtk.Main;
with Gtkada.Builder; use Gtkada.Builder;
with Glib.Error; use Glib.Error;
with Ada.Text_IO;
with Lander;
with Timeline;

procedure Main is

   Glade_Filename : constant String := "resources/station.glade";
   Builder : Gtkada_Builder;
   Error : aliased GError;
begin

   Lander.Reset;
   Timeline.Set_Now (0);
   Timeline.Set_Situation (Lander.Get_Situation);

   Gtk.Main.Init;

   Gtk_New (Builder);

   --  Load glade file
   if Add_From_File (Builder, Glade_Filename, Error'Access) = 0 then
      Ada.Text_IO.Put_Line ("Error : " & Get_Message (Error));
      Error_Free (Error);
      return;
   end if;

   Init_Gtk (Builder);

   Gtk.Main.Main;
end Main;
