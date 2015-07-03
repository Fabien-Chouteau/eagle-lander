with System; use System;
with Glib.Object;
with Pango.Font; use Pango.Font;
with Pango.Cairo; use Pango.Cairo;

package body Text_Utils is

   Layout : Pango_Layout := null;
   Desc   : Pango_Font_Description := null;

   function Create_Layout (Context : Cairo_Context)
                           return Pango_Layout is
      function Internal (Context : Cairo_Context) return Address;
      pragma Import (C, Internal, "pango_cairo_create_layout");
      Stub : Pango_Layout_Record;
   begin
      return Pango_Layout
        (Glib.Object.Get_User_Data (Internal (Context), Stub));
   end Create_Layout;

   function LM_Font
     (Cr      : Cairo_Context;
      Size    : Gdouble;
      Gravity : Pango.Enums.Gravity := Pango.Enums.Pango_Gravity_South)
      return Pango_Layout
   is
   begin
      if Layout = null then
         Layout := Create_Layout (Cr);
      end if;
      if Desc = null then
         Desc := Pango.Font.From_String ("arial bold 9");
      end if;

      Set_Size (Desc, Gint (Size + 1.0) * 750);
      --Set_Gravity (Desc, Gravity);
      Set_Font_Description (Layout, Desc);
      return Layout;
   end LM_Font;

   procedure Draw_Centered_Text
     (Cr      : Cairo_Context;
      Text    : String;
      Pos     : Vector2D;
      Size    : Gdouble;
      Gravity : Pango.Enums.Gravity := Pango.Enums.Pango_Gravity_South)
   is
      Layout       : Pango_Layout;
      Ink_Rect     : Pango_Rectangle;
      Logical_Rect : Pango_Rectangle;
   begin
      Layout := LM_Font (Cr, Size, Gravity);
      Set_Text (Layout, Text);
      Layout.Get_Pixel_Extents (Ink_Rect, Logical_Rect);
      Save (Cr);
      Move_To (Cr, Pos.X - Gdouble (Logical_Rect.Width / 2),
               Pos.Y - Gdouble (Logical_Rect.Height / 2));
      Pango.Cairo.Show_Layout  (Cr, Layout);
      Restore (Cr);
   end Draw_Centered_Text;

   procedure Draw_Right_Text (Cr : Cairo_Context;
                              Text : String;
                              Pos : Vector2D;
                              Size : Gdouble) is
         Layout : Pango_Layout;
         Ink_Rect    : Pango_Rectangle;
         Logical_Rect : Pango_Rectangle;
   begin
      Layout := LM_Font (Cr, Size);
      Set_Text (Layout, Text);
      Layout.Get_Pixel_Extents (Ink_Rect, Logical_Rect);
      Save (Cr);
      Move_To (Cr, Pos.X - Gdouble (Logical_Rect.Width),
               Pos.Y - Gdouble (Logical_Rect.Height / 2));
      Pango.Cairo.Show_Layout  (Cr, Layout);
      Restore (Cr);
   end Draw_Right_Text;
end Text_Utils;
