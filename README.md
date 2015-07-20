# eagle-lander
Apollo 11 lunar lander simulator (Ada/Gtk/Cairo)

To learn more about this simulator: ["The Eagle has landed"](http://blog.adacore.com/make-with-ada-the-eagle-has-landed).

## Build

For this project you will need GNAT and GTKAda. You can find GPL versions at
[libre.adacore.com](http://libre.adacore.com), in the download section. See the
respective documentations for installation.

To build, open the project file lunar_lander.gpr in
[GPS](http://libre.adacore.com/tools/gps/) and click on the "Build All" button,
or run "gprbuild" in the root directory.

### PATH to GTKAda

The path to GTKAda in the lunar_lander.gpr is set to the default installation
path on Windows, you may have to change it for your installation (especially on
Linux).

## Controls

Keyboard:
 * Up : Increase DPS throttle
 * Down : Decrease DPS throttle
 * Right : Decrease RCS throttle (turn clockwise)
 * Left : Increase RCS throttle (turn counterclockwise)
 * Space : Pause simulation
 * Page Up: Zoom in
 * Page Down: Zoom out

On screen:
 * DPS Throttle slider (Middle slider to the left of the screen)
 * RCS Throttle sliders (Left and right sliders to the left of the screen)
 * Pause button (Bottom-Left): Start/Stop the simulation
 * Timeline slider (Bottom): When the simulation is paused you can drag the cursor to go back in time (and maybe fix one or two mistakes...)
 * Reset button (Top-Left): Restart the simulation at High-Gate (position, speed, attitude, fuel)
 * Help button (Top-Left): Enable or disable the help features
