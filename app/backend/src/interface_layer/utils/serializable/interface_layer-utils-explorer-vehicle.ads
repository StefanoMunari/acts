-- local
with Active.Traveller;

with Reactive.District;

with Interface_Layer.Utils.Explorer;

with Shared.Agent_Id_List;
with Shared.Indefinite_String_Map;

package Interface_Layer.Utils.Explorer.Vehicle is

  package Traveller renames Active.Traveller;
  package Explorer renames Interface_Layer.Utils.Explorer;
  package String_Map renames Shared.Indefinite_String_Map;
  package Agent_Id_List renames Shared.Agent_Id_List;

   type Object is abstract
      new Explorer.Object
   with record
      Max_Passengers    : Natural;
      Passengers        : Agent_Id_List.List := Agent_Id_List.Empty_List;
      Passengers_Number : Natural;
      District          : access Reactive.District.Object'Class;
   end record;
   type Reference is access all Vehicle.Object'Class;

   procedure Init (
      This           : in out Vehicle.Object'Class;
      Max_Passengers :        Natural;
      Passengers     :        Agent_Id_List.List;
      District       : access Reactive.District.Object'Class := null);

   procedure Set_District (
      This     : in out Vehicle.Object'Class;
      District : access Reactive.District.Object'Class := null);

   overriding
   procedure Marshalling (
      This       : in     Explorer.Vehicle.Object;
      Stream_Map :    out String_Map.Data.Map);

   overriding
   procedure Unmarshalling (
      This       : in out Explorer.Vehicle.Object;
      Stream_Map : in     String_Map.Data.Map);

end Interface_Layer.Utils.Explorer.Vehicle;
