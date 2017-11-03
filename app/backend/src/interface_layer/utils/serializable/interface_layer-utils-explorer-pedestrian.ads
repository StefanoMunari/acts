with Active.Traveller;

with Interface_Layer.Utils.Explorer;

with Shared.Indefinite_String_Map;

package Interface_Layer.Utils.Explorer.Pedestrian is

  package Traveller renames Active.Traveller;
  package Explorer renames Interface_Layer.Utils.Explorer;
  package String_Map renames Shared.Indefinite_String_Map;

   type Object is
     new Explorer.Object
   with null record;
   type Reference is access all Pedestrian.Object'Class;

   overriding
   procedure Marshalling (
    This       : in     Pedestrian.Object;
    Stream_Map :    out String_Map.Data.Map);

   overriding
   procedure Unmarshalling (
    This       : in out Pedestrian.Object;
    Stream_Map : in     String_Map.Data.Map);

end Interface_Layer.Utils.Explorer.Pedestrian;
