with Active.Agent;
with Active.People_Carrier;

limited with Reactive.District.Mock;

with Shared.Agent_Boolean_Map;

package Active.People_Carrier.Utils.Mock is

   package Agent renames Active.Agent;
   package Agent_Boolean_Map renames Shared.Agent_Boolean_Map;

   type Object (<>) is new People_Carrier.Utils.Object with private;
   type Reference is access all People_Carrier.Utils.Mock.Object'Class;

   function Create return People_Carrier.Utils.Mock.Reference;

   overriding
   function Is_A_People_Carrier (This         : in     Mock.Object;
                                 Traveller_Id : in     Agent.Agent_Id)
   return Boolean;

   overriding
   function Get_Passengers (This       : in     Mock.Object;
                            Carrier_Id : in     Agent.Agent_Id)
   return Agent_Id_List.List;

   overriding
   procedure Board (This       : in     Mock.Object;
                    Carrier_Id : in     Agent.Agent_Id;
                    Incomer_Id : in     Agent.Agent_Id;
                    Boarded    :    out Boolean);

   overriding
   procedure Free (This         : in out Mock.Object;
                   Carrier_Id   : in     Agent.Agent_Id;
                   Passenger_Id : in     Agent.Agent_Id;
                   Freed        :    out Boolean);

   overriding
   function Is_Carrier_Full (This       : in out Mock.Object;
                             Carrier_Id : in     Agent.Agent_Id)
    return Boolean;

   not overriding
   procedure Set_Value_For_Is_A_People_Carrier (
      This                : in out Mock.Object;
      Traveller_Id        : in     Agent.Agent_Id;
      Is_A_People_Carrier : in     Boolean);

   not overriding
   procedure Set_List_For_Get_Passengers (
      This       : in out Mock.Object;
      Passengers : in     Agent_Id_List.List);

   not overriding
   procedure Set_Value_For_Board (
      This       : in out Mock.Object;
      Boarder_Id : in     Agent.Agent_Id;
      Boarded    : in     Boolean);

   not overriding
   procedure Set_Value_For_Free (
      This         : in out Mock.Object;
      Passenger_Id : in     Agent.Agent_Id;
      Freed        : in     Boolean);

   not overriding
   procedure Set_Value_For_Is_Carrier_Full (
      This       : in out Mock.Object;
      Carrier_Id : in     Agent.Agent_Id;
      Full       : in     Boolean);

   not overriding
   function Get_Free_Called (This  : in out Mock.Object) return Boolean;

private
-- TODO: Improve tests by using maps
   type Return_Values_Collection is record
      Is_A_People_Carrier : Agent_Boolean_Map.Map;
      Is_A_People_Carrier_Existence : Boolean := False;
      Passengers : Agent_Id_List.List;
      Passengers_Existence : Boolean := False;
      Boarded : Agent_Boolean_Map.Map;
      Boarded_Existence : Boolean := FALSE;
      Freed : Agent_Boolean_Map.Map;
      Freed_Existence : Boolean := FALSE;
      Free_Called : Boolean := False;
      Is_Carrier_Full : Agent_Boolean_Map.Map;
      Is_Carrier_Full_Existence : Boolean := False;
   end record;

   type Object is new People_Carrier.Utils.Object with record
      Return_Values : Return_Values_Collection;
   end record;

end Active.People_Carrier.Utils.Mock;
