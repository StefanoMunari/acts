-- local
with Active.Traveller;
with Active.Traveller.Vehicle.Private_Motor_Vehicle;

with Interface_Layer.Utils.Explorer.Vehicle;

with Shared.Indefinite_String_Map;

package Interface_Layer.Utils.Explorer.Vehicle.Private_Motor_Vehicle is

   package Traveller        renames Active.Traveller;
   package PMV
      renames Active.Traveller.Vehicle.Private_Motor_Vehicle;
   package Explorer_Vehicle renames Interface_Layer.Utils.Explorer.Vehicle;
   package String_Map       renames Shared.Indefinite_String_Map;

   type Object is new Vehicle.Object with record
      Vehicle_Type : PMV.Private_Motor_Vehicle_Type;
   end record;
   type Reference is access all Private_Motor_Vehicle.Object'Class;

   procedure Init (This         : in out Private_Motor_Vehicle.Object'Class;
                   Vehicle_Type : in     PMV.Private_Motor_Vehicle_Type);
   overriding
   procedure Marshalling (
      This       : in Private_Motor_Vehicle.Object;
      Stream_Map : out String_Map.Data.Map);
   overriding
   procedure Unmarshalling (
      This       : in out Private_Motor_Vehicle.Object;
      Stream_Map : in     String_Map.Data.Map);

end Interface_Layer.Utils.Explorer.Vehicle.Private_Motor_Vehicle;
