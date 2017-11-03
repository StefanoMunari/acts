with Active.Traveller.Pedestrian;

package body Interface_Layer.Utils.Explorer.Vehicle.Bicycle is

   procedure Marshalling (
      This       : in     Bicycle.Object;
      Stream_Map :    out String_Map.Data.Map)
   is
   begin
      Vehicle.Marshalling (Vehicle.Object (This), Stream_Map);
   end Marshalling;

   procedure Unmarshalling (
      This       : in out Bicycle.Object;
      Stream_Map : in     String_Map.Data.Map)
   is
   begin
      Vehicle.Object (This).Unmarshalling (Stream_Map);
   end Unmarshalling;

end Interface_Layer.Utils.Explorer.Vehicle.Bicycle;
