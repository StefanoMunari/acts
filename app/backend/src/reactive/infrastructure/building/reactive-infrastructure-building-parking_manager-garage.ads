with Ada.Containers.Ordered_Maps;

with Active.Agent;
with Active.People_Carrier.Utils;
with Active.Traveller.Utils;

with Shared.Agent_Id_To_Agent_Id_Map;
with Shared.Agent_Id_Set;
with Shared.Slice;

package Reactive.Infrastructure.Building.Parking_Manager.Garage is

   package Agent
      renames Active.Agent;
   package People_Carrier_Utils
      renames Active.People_Carrier.Utils;
   package Traveller_Utils
      renames Active.Traveller.Utils;
   package Agent_Id_To_Agent_Id_Map
      renames Shared.Agent_Id_To_Agent_Id_Map;
   package Agent_Id_Set
      renames Shared.Agent_Id_Set;
   package Slice
      renames Shared.Slice;

   type Object is
      new Parking_Manager.Object
   with private;
   type Reference is access all Object'Class;

   not overriding
   function Create (
      T_Utils  : access Traveller_Utils.Object'Class := null;
      PC_Utils : access People_Carrier_Utils.Object'Class := null)
   return Garage.Reference;

   overriding
   procedure Park_Vehicle (This       : in out Garage.Object;
                           Vehicle_Id : in     Agent.Agent_Id);

-- CAUTION: Used only at build time and by callbacks
   overriding
   procedure Put_Leaving_Vehicle (
      This       : in out Garage.Object;
      Driver_Id  : in     Agent.Agent_Id;
      Vehicle_Id : in     Agent.Agent_Id);

-- CAUTION: Used only at build time
   overriding
   procedure Put_Pending_Vehicle (
      This       : in out Garage.Object;
      Vehicle_Id : in     Agent.Agent_Id);

-- CAUTION: Used only by callbacks
   not overriding
   procedure Remove_Pending_Vehicle (
      This       : in out Garage.Object;
      Vehicle_Id : in     Agent.Agent_Id;
      Driver_Id  : in     Agent.Agent_Id);

   overriding
   procedure Ask_For_Vehicle (This         : in out Garage.Object;
                              Traveller_Id : in      Agent.Agent_Id;
                              Vehicle_Id   :    out Agent.Agent_Id;
                              Boarded      :    out Boolean);

   overriding
   function Leave_Parking (This         : in out Garage.Object;
                           Passenger_Id : in     Agent.Agent_Id;
                           Vehicle_Id   :    out Agent.Agent_Id)
   return Boolean;

   overriding
   function Is_A_Driver (This         : in out Garage.Object;
                         Traveller_Id : in     Agent.Agent_Id)
   return Boolean;

   procedure Unboard_Vehicle (
      This             : in out Garage.Object;
      Vehicle_Id       : in     Agent.Agent_Id;
      Driver_Id        : in     Agent.Agent_Id);

   overriding
   function Book_Parking (This       : in out Garage.Object;
                          Vehicle_Id : in     Agent.Agent_Id)
   return Boolean;

   overriding
   procedure Set_Host_Id (This    : in out Garage.Object;
                          Host_Id : in     Infra_Id);

   function Get_Host_Id (This    : in out Garage.Object)
   return Infra_Id;

   overriding
   procedure Set_Size (This : in out Garage.Object;
                       Size : in     Natural);

   overriding
   function Dump (This : Garage.Object) return G_JSON.JSON_Value;

private

   type Parked_Vehicles;
   type Pending_Vehicles;

   protected type Leaving_Vehicles is

      procedure Add_Vehicle (Vehicle_Id  : in Agent.Agent_Id;
                             Driver_Id   : in Agent.Agent_Id);

      procedure Board_Vehicle (
         Vehicle_Id   :    out Agent.Agent_Id;
         Traveller_Id : in     Agent.Agent_Id;
         Destination  : in     Slice.Map;
         Boarded      :    out Boolean);

      function Is_The_Driver (Traveller_Id : in Agent.Agent_Id)
      return Boolean;

      procedure Leave (Traveller_Id : in     Agent.Agent_Id;
                       Vehicle_Id   :    out Agent.Agent_Id);

      function Count return Natural;

      function Dump return G_JSON.JSON_Array;

      procedure Set_Traveller_Utils (
         T_Utils_Arg : access Traveller_Utils.Object'Class := null);

      procedure Set_People_Carrier_Utils (
         PC_Utils_Arg : access People_Carrier_Utils.Object'Class := null);

   private
      Vehicles : Agent_Id_Set.Set := Agent_Id_Set.Empty_Set;
      Drivers  : Agent_Id_To_Agent_Id_Map.Map;
      T_Utils  : access Traveller_Utils.Object'Class := null;
      PC_Utils : access People_Carrier_Utils.Object'Class := null;
   end Leaving_Vehicles;

   protected type Parked_Vehicles is

      procedure Add_Vehicle (Vehicle_Id : in     Agent.Agent_Id;
                             Parked     :    out Boolean);

      procedure Board_Vehicle (
         Vehicle_Id       :    out Agent.Agent_Id;
         Traveller_Id     : in     Agent.Agent_Id;
         Pending_Vehicles : access Garage.Pending_Vehicles;
         Found            :    out Boolean);

      procedure Unboard_Vehicle (
         Pending_Vehicles : access Garage.Pending_Vehicles;
         Vehicle_Id       : in     Agent.Agent_Id;
         Driver_Id        : in     Agent.Agent_Id);

      procedure Book (
         Leaving_Vehicles : access Garage.Leaving_Vehicles;
         Pending_Vehicles : access Garage.Pending_Vehicles;
         Vehicle_Id       : in     Agent.Agent_Id;
         Booked           :    out Boolean);

      procedure Set_Host_Id (New_Host_Id : in Infra_Id);

      function Get_Host_Id return Infra_Id;

      procedure Set_Size (New_Size : in Natural);

      function Get_Size return Natural;

      procedure Set_Traveller_Utils (
         T_Utils_Arg : access Traveller_Utils.Object'Class := null);

      function Contains (Vehicle_Id : in Agent.Agent_Id) return Boolean;

      function Dump return G_JSON.JSON_Array;

   private
      Size     : Natural;
      Host_Id  : Infra_Id;
      Vehicles : Agent_Id_Set.Set := Agent_Id_Set.Empty_Set;
      T_Utils  : access Traveller_Utils.Object'Class := null;
   end Parked_Vehicles;

   protected type Pending_Vehicles is

      procedure Add_Vehicle (Vehicle_Id : in     Agent.Agent_Id;
                             Room       : in     Natural;
                             Booked     :    out Boolean);

      procedure Put_Vehicle (Vehicle_Id : in Agent.Agent_Id);

      procedure Put_Vehicle (
         Vehicle_Id : in Agent.Agent_Id;
         Driver_Id  : in Agent.Agent_Id);

      procedure Remove_Vehicle (Vehicle_Id : in Agent.Agent_Id);

      procedure Remove_Vehicle (
         Vehicle_Id : in Agent.Agent_Id;
         Driver_Id  : in Agent.Agent_Id);

      function Is_A_Driver (Traveller_Id : in Agent.Agent_Id)
      return Boolean;

      function Dump return G_JSON.JSON_Array;

   private
      Vehicles : Agent_Id_Set.Set := Agent_Id_Set.Empty_Set;
      Drivers  : Agent_Id_Set.Set := Agent_Id_Set.Empty_Set;
   end Pending_Vehicles;

   type Object is
      new Parking_Manager.Object
   with record
      Leaving_Vehicles : access Garage.Leaving_Vehicles;
      Parked_Vehicles  : access Garage.Parked_Vehicles;
      Pending_Vehicles : access Garage.Pending_Vehicles;
      T_Utils          : access Traveller_Utils.Object'Class := null;
      PC_Utils         : access People_Carrier_Utils.Object'Class := null;
   end record;

end Reactive.Infrastructure.Building.Parking_Manager.Garage;
