with Active.Traveller;

with Interface_Layer.Utils.Explorer.Vehicle;

with Shared.Indefinite_String_Map;

package Interface_Layer.Utils.Explorer.Vehicle.Bicycle is

  package Traveller        renames Active.Traveller;
  package Explorer_Vehicle renames Interface_Layer.Utils.Explorer.Vehicle;
  package String_Map       renames Shared.Indefinite_String_Map;

   type Object is new Explorer_Vehicle.Object with null record;
   type Reference is access all Bicycle.Object'Class;

   overriding
   procedure Marshalling (
      This       : in     Bicycle.Object;
      Stream_Map :    out String_Map.Data.Map);
   overriding
   procedure Unmarshalling (
      This       : in out Bicycle.Object;
      Stream_Map : in     String_Map.Data.Map);

end Interface_Layer.Utils.Explorer.Vehicle.Bicycle;
