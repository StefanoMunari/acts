with Active.Traveller;

with Interface_Layer.Utils.Explorer.Vehicle;

with Shared.Indefinite_String_Map;

package Interface_Layer.Utils.Explorer.Vehicle.Bus is

   package Traveller_Pkg    renames Active.Traveller;
   package Explorer_Vehicle renames Interface_Layer.Utils.Explorer.Vehicle;
   package String_Map       renames Shared.Indefinite_String_Map;

   type Object is new Explorer_Vehicle.Object with record
      Bus_Stops   : Infra_Id_List.List;
      Route_Stops : Infra_Id_List.List;
   end record;
   type Reference is access all Bus.Object'Class;

   procedure Init (
      This        : in out Bus.Object;
      Bus_Stops   : in     Infra_Id_List.List;
      Route_Stops : in     Infra_Id_List.List);

   overriding
   procedure Marshalling (
      This       : in     Bus.Object;
      Stream_Map :    out String_Map.Data.Map);

   overriding
   procedure Unmarshalling (
      This       : in out Bus.Object;
      Stream_Map : in     String_Map.Data.Map);

end Interface_Layer.Utils.Explorer.Vehicle.Bus;
