-- local
with Shared.Indefinite_String_Map;
with Shared.String_List;
with Interface_Layer.Presentation.JSON_Format;

package Shared.JSON_Splitter is

   package JSON_Format renames Interface_Layer.Presentation.JSON_Format;
   package String_Map renames Shared.Indefinite_String_Map;
   package String_List renames Shared.String_List;

   function Split (Input : JSON_Format.Object) return String_Map.Data.Map;

private
   function Extract_Keys (Raw_Input : String) return String_List.List;
   function Is_JSON_Array (Raw_Input : String) return Boolean;

end Shared.JSON_Splitter;
