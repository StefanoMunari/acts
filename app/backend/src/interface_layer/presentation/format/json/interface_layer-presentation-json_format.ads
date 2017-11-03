-- local
with Interface_Layer.Presentation.XFormat;
-- library
with GNATCOLL.JSON;

package Interface_Layer.Presentation.JSON_Format is

   package XFormat renames Interface_Layer.Presentation.XFormat;
   package G_JSON renames GNATCOLL.JSON;

   type Object is
    new G_JSON.JSON_Value
    and XFormat.Object
   with null record;

   type Reference is access all JSON_Format.Object'Class;

   overriding
   procedure Set_Header (Message : in out JSON_Format.Object;
                         Header  : in      XFormat.Reference);

   overriding
   procedure Set_Payload (Message : in out JSON_Format.Object;
                          Payload : in     XFormat.Reference);

   overriding
   function Get_Header (Message : in out JSON_Format.Object)
   return XFormat.Object'Class;

   overriding
   function Get_Payload (Message : in out JSON_Format.Object)
   return XFormat.Object'Class;

   overriding
   function "=" (A, B : JSON_Format.Object) return Boolean;

end Interface_Layer.Presentation.JSON_Format;
