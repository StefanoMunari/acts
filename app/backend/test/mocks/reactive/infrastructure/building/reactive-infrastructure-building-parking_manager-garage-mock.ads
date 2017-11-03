with Active.Agent;

with Shared.Agent_Boolean_Map;
with Shared.Agent_Id_List;

package Reactive.Infrastructure.Building.Parking_Manager.Garage.Mock is

   package Agent             renames Active.Agent;
   package Agent_Boolean_Map renames Shared.Agent_Boolean_Map;
   package Agent_Id_List     renames Shared.Agent_Id_List;

   type Object (<>) is new Garage.Object with private;
   type Reference is access all Garage.Mock.Object'Class;

   function Create return Parking_Manager.Reference;

   overriding
   procedure Park_Vehicle (This         : in out Garage.Mock.Object;
                           Traveller_Id : in     Agent.Agent_Id);

   overriding
   procedure Put_Leaving_Vehicle (
      This       : in out Parking_Manager.Garage.Mock.Object;
      Driver_Id  : in     Agent.Agent_Id;
      Vehicle_Id : in     Agent.Agent_Id) is null;

   overriding
   procedure Put_Pending_Vehicle (
      This       : in out Parking_Manager.Garage.Mock.Object;
      Vehicle_Id : in     Agent.Agent_Id) is null;

   overriding
   procedure Ask_For_Vehicle (This          : in out Mock.Object;
                              Traveller_Id  : in     Agent.Agent_Id;
                              Vehicle_Id    :    out Agent.Agent_Id;
                              Vehicle_Found :    out Boolean);

   overriding
   function Leave_Parking (This         : in out Garage.Mock.Object;
                           Passenger_Id : in     Agent.Agent_Id;
                           Vehicle_Id   :    out Agent.Agent_Id)
   return Boolean;

   overriding
   function Is_A_Driver (This         : in out Garage.Mock.Object;
                         Traveller_Id : in     Agent.Agent_Id)
   return Boolean;

   overriding
   function Book_Parking (This       : in out Garage.Mock.Object;
                          Vehicle_Id : in     Agent.Agent_Id)
   return Boolean;

   overriding
   procedure Set_Host_Id (This    : in out Parking_Manager.Garage.Mock.Object;
                          Host_Id : in     Infra_Id);

   overriding
   procedure Set_Size (This : in out Parking_Manager.Garage.Mock.Object;
                       Size : in     Natural);

   overriding
   function Dump (This : Garage.Mock.Object) return G_JSON.JSON_Value;

   not overriding
   procedure Set_Values_For_Ask_For_Vehicle (
      This          : in out Parking_Manager.Garage.Mock.Object;
      Vehicle_Id    : in     Agent.Agent_Id;
      Vehicle_Found : in     Boolean);

   not overriding
   procedure Set_Values_For_Leave_Parking (
      This       : in out Parking_Manager.Garage.Mock.Object;
      Leaving_Id : in     Agent.Agent_Id;
      Is_Leaving : in     Boolean);

   not overriding
   procedure Set_Values_For_Book_Parking (
      This       : in out Parking_Manager.Garage.Mock.Object;
      Booking_Id : in     Agent.Agent_Id;
      Is_Booking : in     Boolean);

   not overriding
   procedure Set_Values_For_Is_A_Driver (
      This         : in out Parking_Manager.Garage.Mock.Object;
      Return_Value : in     Boolean);

   not overriding
   procedure Set_Return_Value_For_Dump (
      This         : in out Parking_Manager.Garage.Mock.Object;
      Return_Value : in     G_JSON.JSON_Value);

   not overriding
   function Get_Park_Vehicle_Called (
      This : in out Parking_Manager.Garage.Mock.Object) return Boolean;

   not overriding
   function Get_Set_Host_Id (
      This : in out Parking_Manager.Garage.Mock.Object) return Infra_Id;

   not overriding
   function Get_Set_Size (
      This : in out Parking_Manager.Garage.Mock.Object) return Natural;

private
   type Mock_Values_Collection is record
      Park_Vehicle_Called : Boolean := False;
      Set_Host_Id_Value : Infra_Id;
      Set_Size_Value : Natural;
   end record;

-- TODO: It may be useful to refactor this by using a map
   type Return_Values_Collection is record
      Vehicle_Id              : Agent.Agent_Id;
      Vehicle_Id_Existence    : Boolean := False;
      Vehicle_Found           : Boolean;
      Vehicle_Found_Existence : Boolean := False;
      Leave_Parking_Id        : Agent.Agent_Id;
      Leave_Parking_Bool      : Boolean;
      Leave_Parking_Existence : Boolean := False;
      Book_Parking_Id         : Agent.Agent_Id;
      Book_Parking_Bool       : Boolean;
      Book_Parking_Existence  : Boolean := False;
      Is_A_Driver             : Boolean;
      Is_A_Driver_Existence   : Boolean := False;
      Dump                    : G_JSON.JSON_Value;
      Dump_Existence          : Boolean := False;
   end record;

   type Object is new Garage.Object with record
      Mock_Values   : Mock_Values_Collection;
      Return_Values : Return_Values_Collection;
   end record;

end Reactive.Infrastructure.Building.Parking_Manager.Garage.Mock;
