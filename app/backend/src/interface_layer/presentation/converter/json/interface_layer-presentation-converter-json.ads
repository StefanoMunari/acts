-- local
with Interface_Layer.Presentation.XFormat;
with Interface_Layer.Presentation.JSON_Format;

with Shared.JSON_Array_Ref_Hashed_Map;
with Shared.JSON_Array_Utils;
with Shared.Indefinite_String_Map;

package Interface_Layer.Presentation.Converter.JSON is

   package XFormat             renames Interface_Layer.Presentation.XFormat;
   package JSON_Format
      renames Interface_Layer.Presentation.JSON_Format;
   package JSON_Array_Ref_HMap renames Shared.JSON_Array_Ref_Hashed_Map;
   package Utils               renames Shared.JSON_Array_Utils;
   package String_Map          renames Shared.Indefinite_String_Map;

   type Object is
     new Converter.Object
   with null record;
   type Reference is access all JSON.Object'Class;

   overriding -- use JSON_Format as dynamic type of return Object
   function Encode (
      This : in JSON.Object;
      Raw  : in String_Map.Data.Map)
   return XFormat.Object'Class;

   overriding -- use JSON_Format as dynamic type of the input message
   function Decode (
      This    : in JSON.Object;
      Message : in XFormat.Object'Class)
   return String_Map.Data.Map;

private
   procedure Parse_Raw (This        : in     JSON.Object;
                        Pair        : in     String_Map.Data.Cursor;
                        Separator   : in     String;
                        Accumulator : in out JSON_Array_Ref_HMap.Map);
   function Parse_Key (
      This      : in JSON.Object;
      Raw_Key   : in String;
      Separator : in String)
   return String;

   function Parse_List (
      This      : in JSON.Object;
      Raw_List  : in String;
      Separator : in String)
   return Utils.JSON_Array_Ref;

end Interface_Layer.Presentation.Converter.JSON;
