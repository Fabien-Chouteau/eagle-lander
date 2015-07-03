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
