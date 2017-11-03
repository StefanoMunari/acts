with Active.Agent;

package Reactive.Infrastructure.Building.Parking_Manager is

   package Agent renames Active.Agent;

   type Object is interface;
   type Reference is access all Object'Class;

   not overriding
   procedure Park_Vehicle (This         : in out Parking_Manager.Object;
                           Traveller_Id : in     Agent.Agent_Id) is abstract;

   not overriding
   procedure Put_Leaving_Vehicle (
      This       : in out Parking_Manager.Object;
      Driver_Id  : in     Agent.Agent_Id;
      Vehicle_Id : in     Agent.Agent_Id) is abstract;

   not overriding
   procedure Put_Pending_Vehicle (
      This       : in out Parking_Manager.Object;
      Vehicle_Id : in     Agent.Agent_Id) is abstract;

   not overriding
   procedure Ask_For_Vehicle (This         : in out Parking_Manager.Object;
                              Traveller_Id : in     Agent.Agent_Id;
                              Vehicle_Id   :    out Agent.Agent_Id;
                              Boarded      :    out Boolean) is abstract;

-- returns true if vehicle left parking
   not overriding
   function Leave_Parking (This         : in out Parking_Manager.Object;
                           Passenger_Id : in     Agent.Agent_Id;
                           Vehicle_Id   :    out Agent.Agent_Id)
   return Boolean is abstract;

   not overriding
   function Is_A_Driver (This         : in out Parking_Manager.Object;
                         Traveller_Id : in     Agent.Agent_Id)
   return Boolean is abstract;

   not overriding
   procedure Set_Host_Id (This    : in out Parking_Manager.Object;
                          Host_Id : in     Infra_Id)
   is abstract;

   not overriding
   procedure Set_Size (This : in out Parking_Manager.Object;
                       Size : in     Natural)
   is abstract;

   not overriding
   function Book_Parking (This       : in out Parking_Manager.Object;
                          Vehicle_Id : in     Agent.Agent_Id)
   return Boolean
   is abstract;

   not overriding
   function Dump (This : Parking_Manager.Object) return G_JSON.JSON_Value
   is abstract;

-- JSON FIELDS CONSTANTS
   function Capacity_Field return String is ("capacity");
   function Parked_Field   return String is ("parkedVehicles");
   function Leaving_Field  return String is ("leavingVehicles");
   function Pending_Field  return String is ("pendingVehicles");
   function Vehicle_Field  return String is ("vehicle");
   function Driver_Field   return String is ("driver");

end Reactive.Infrastructure.Building.Parking_Manager;
