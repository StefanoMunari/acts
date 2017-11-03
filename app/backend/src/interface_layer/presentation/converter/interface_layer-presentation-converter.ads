-- local
with Interface_Layer.Presentation.XFormat;
with Shared.Indefinite_String_Map;

package Interface_Layer.Presentation.Converter is

   package String_Map renames Shared.Indefinite_String_Map;
   package XFormat renames Interface_Layer.Presentation.XFormat;

   type Object is interface;
   type Reference is access all Converter.Object'Class;

   function Encode (
      This : in Converter.Object;
      Raw  : in String_Map.Data.Map)
   return XFormat.Object'Class is abstract;

   function Decode (
      This    : in Converter.Object;
      Message : in XFormat.Object'Class)
   return String_Map.Data.Map is abstract;

end Interface_Layer.Presentation.Converter;
