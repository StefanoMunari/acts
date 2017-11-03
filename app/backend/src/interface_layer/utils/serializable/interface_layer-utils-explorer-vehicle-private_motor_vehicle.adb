with Active.Traveller.Pedestrian;

package body Interface_Layer.Utils.Explorer.Vehicle.Private_Motor_Vehicle is

   procedure Init (This         : in out Private_Motor_Vehicle.Object'Class;
                   Vehicle_Type : in     PMV.Private_Motor_Vehicle_Type)
   is
   begin
     This.Vehicle_Type := Vehicle_Type;
   end Init;

   procedure Marshalling (
    This       : in     Private_Motor_Vehicle.Object;
    Stream_Map :    out String_Map.Data.Map) is
   begin
      Vehicle.Object (This).Marshalling (Stream_Map);
      -- Serialize Private_Motor_Vehicle_Type
      Stream_Map.Insert (
        "Vehicle_Type",
        PMV.Private_Motor_Vehicle_Type'Image (This.Vehicle_Type));
   end Marshalling;

   procedure Unmarshalling (
    This       : in out Private_Motor_Vehicle.Object;
    Stream_Map : in     String_Map.Data.Map) is
   begin
      Vehicle.Object (This).Unmarshalling (Stream_Map);
      This.Vehicle_Type :=
        PMV.Private_Motor_Vehicle_Type'Value (
          Stream_Map.Element ("Vehicle_Type"));
   end Unmarshalling;

end Interface_Layer.Utils.Explorer.Vehicle.Private_Motor_Vehicle;
