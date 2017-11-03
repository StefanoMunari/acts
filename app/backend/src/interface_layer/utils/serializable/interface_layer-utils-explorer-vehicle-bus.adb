with Active.Traveller.Pedestrian;

with Shared.String_Splitter;
with Shared.String_List;

package body Interface_Layer.Utils.Explorer.Vehicle.Bus is

   package SSplitter   renames Shared.String_Splitter;
   package String_List renames Shared.String_List;

   procedure Init (
      This        : in out Bus.Object;
      Bus_Stops   : in     Infra_Id_List.List;
      Route_Stops : in     Infra_Id_List.List) is
   begin
      This.Bus_Stops := Bus_Stops;
      This.Route_Stops := Route_Stops;
   end Init;

   procedure Marshalling (
      This       : in     Bus.Object;
      Stream_Map :    out String_Map.Data.Map)
   is
      Bus_Stops_SU   : SU.Unbounded_String;
      Route_Stops_SU : SU.Unbounded_String;
   begin
      Explorer.Vehicle.Object (This).Marshalling (Stream_Map);
      Bus_Stops_SU   := Encode_List (This.Bus_Stops);
      Route_Stops_SU := Encode_List (This.Route_Stops);
      Stream_Map.Insert ("busStops", SU.To_String (Bus_Stops_SU));
      Stream_Map.Insert ("routeStops", SU.To_String (Route_Stops_SU));
   end Marshalling;

   procedure Unmarshalling (
      This       : in out Bus.Object;
      Stream_Map : in     String_Map.Data.Map)
   is
      Bus_Stops_List_Str   : String_List.List := SSplitter.Filter_Separator (
         Stream_Map.Element ("busStops"), Separator);
      Route_Stops_List_Str : String_List.List := SSplitter.Filter_Separator (
         Stream_Map.Element ("routeStops"), Separator);
   begin

      Explorer.Vehicle.Object (This).Unmarshalling (Stream_Map);

      for Bus_Stop of Bus_Stops_List_Str loop
         This.Bus_Stops.Append (Infra_Id (Integer'Value (Bus_Stop)));
      end loop;

      for Route_Stop of Route_Stops_List_Str loop
         This.Route_Stops.Append (Infra_Id (Integer'Value (Route_Stop)));
      end loop;
   end Unmarshalling;

end Interface_Layer.Utils.Explorer.Vehicle.Bus;
