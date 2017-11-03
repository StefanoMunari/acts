with Shared.Indefinite_String_Map;
with Interface_Layer.Wrappers.InterfaceL;

package Interface_Layer.Containers.Pair is

   package String_Map renames Shared.Indefinite_String_Map;
   package Interface_Wrapper renames Interface_Layer.Wrappers.InterfaceL;

   type Object is tagged
   record
      First  : String_Map.Data.Map;
      Second : Interface_Wrapper.Object;
   end record;
   type Reference is access all Pair.Object'Class;

   function Create (First : String_Map.Data.Map; Second : Interface_Wrapper.Object) return Pair.Object;

end Interface_Layer.Containers.Pair;