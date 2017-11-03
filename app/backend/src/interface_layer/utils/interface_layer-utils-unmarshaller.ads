with Active.Traveller;

with Shared.Indefinite_String_Map;

package Interface_Layer.Utils.Unmarshaller is

   package Traveller renames Active.Traveller;

   type Object is interface;
   type Reference is access all Unmarshaller.Object'Class;

   procedure Unmarshalling (
      This       : in out Unmarshaller.Object;
      Stream_Map : in     Shared.Indefinite_String_Map.Data.Map)
   is abstract;

end Interface_Layer.Utils.Unmarshaller;
