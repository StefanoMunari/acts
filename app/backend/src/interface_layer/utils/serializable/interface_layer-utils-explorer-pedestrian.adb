with Active.Traveller.Pedestrian;

package body Interface_Layer.Utils.Explorer.Pedestrian is

   procedure Marshalling (
      This       : in     Pedestrian.Object;
      Stream_Map :    out String_Map.Data.Map) is
   begin
      Explorer.Object (This).Marshalling (Stream_Map);
   end Marshalling;

   procedure Unmarshalling (
      This       : in out Pedestrian.Object;
      Stream_Map : in     String_Map.Data.Map) is
   begin
      Explorer.Object (This).Unmarshalling (Stream_Map);
   end Unmarshalling;

end Interface_Layer.Utils.Explorer.Pedestrian;
