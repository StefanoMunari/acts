with Ada.Strings.Unbounded;

with Active.Travel;
with Active.Travel.Travel_Planning;

with Shared.Infra_Id_List;
with Shared.Slice;

package body Reactive.Infrastructure.Building.Parking_Manager.Garage is

   package SU            renames Ada.Strings.Unbounded;
   package Travel_Pkg    renames Active.Travel;
   package Infra_Id_List renames Shared.Infra_Id_List;

   use Agent_Id_Set;
   use Slice;
   use Infra_Id_List;
   use Traveller_Utils;

   function Create (
      T_Utils  : access Traveller_Utils.Object'Class := null;
      PC_Utils : access People_Carrier_Utils.Object'Class := null)
   return Garage.Reference
   is
      Garage_Instance : Garage.Reference := new Garage.Object;
   begin

      Garage_Instance.T_Utils := T_Utils;
      if Garage_Instance.T_Utils = null then
         Garage_Instance.T_Utils := Traveller_Utils.Get_Instance;
      end if;

      Garage_Instance.PC_Utils := PC_Utils;
      if Garage_Instance.PC_Utils = null then
         Garage_Instance.PC_Utils := People_Carrier_Utils.Get_Instance;
      end if;

   -- leaving vehicles
      Garage_Instance.Leaving_Vehicles := new Garage.Leaving_Vehicles;
      Garage_Instance.Leaving_Vehicles.Set_Traveller_Utils (
         Garage_Instance.T_Utils);
      Garage_Instance.Leaving_Vehicles.Set_People_Carrier_Utils (
         Garage_Instance.PC_Utils);

   -- parked vehicles
      Garage_Instance.Parked_Vehicles  := new Garage.Parked_Vehicles;
      Garage_Instance.Parked_Vehicles.Set_Traveller_Utils (
         Garage_Instance.T_Utils);

   -- pending vehicles
      Garage_Instance.Pending_Vehicles  := new Garage.Pending_Vehicles;

      return Garage_Instance;
   end Create;

   overriding
   procedure Park_Vehicle (This       : in out Garage.Object;
                           Vehicle_Id : in     Agent.Agent_Id)
   is
      Parked : Boolean;
   begin
      This.Parked_Vehicles.Add_Vehicle (Vehicle_Id, Parked);
      This.Pending_Vehicles.Remove_Vehicle (Vehicle_Id);
   end Park_Vehicle;

   procedure Put_Leaving_Vehicle (
      This       : in out Garage.Object;
      Driver_Id  : in     Agent.Agent_Id;
      Vehicle_Id : in     Agent.Agent_Id) is
   begin
      This.Leaving_Vehicles.Add_Vehicle (Driver_Id  => Driver_Id,
                                         Vehicle_Id => Vehicle_Id);
   end Put_Leaving_Vehicle;

   procedure Put_Pending_Vehicle (
      This       : in out Garage.Object;
      Vehicle_Id : in     Agent.Agent_Id) is
   begin
      This.Pending_Vehicles.Put_Vehicle (Vehicle_Id => Vehicle_Id);
   end Put_Pending_Vehicle;

   procedure Remove_Pending_Vehicle (
      This       : in out Garage.Object;
      Vehicle_Id : in     Agent.Agent_Id;
      Driver_Id  : in     Agent.Agent_Id) is
   begin
      This.Pending_Vehicles.Remove_Vehicle (
         Vehicle_Id => Vehicle_Id,
         Driver_Id  => Driver_Id);
   end Remove_Pending_Vehicle;

   procedure Ask_For_Vehicle (This         : in out Garage.Object;
                              Traveller_Id : in     Agent.Agent_Id;
                              Vehicle_Id   :    out Agent.Agent_Id;
                              Boarded      :    out Boolean)
   is
      Destination_Slice : Slice.Map :=
         This.T_Utils.Get_Travel_Destination (Traveller_Id);
   begin
   -- Check if there is any leaving vehicle before requesting a parked one
      This.Leaving_Vehicles.Board_Vehicle (Vehicle_Id   => Vehicle_Id,
                                           Traveller_Id => Traveller_Id,
                                           Destination  => Destination_Slice,
                                           Boarded      => Boarded);
      if Boarded then
         return;
      end if;

   -- Then look among parked ones
      This.Parked_Vehicles.Board_Vehicle (
         Vehicle_Id       => Vehicle_Id,
         Traveller_Id     => Traveller_Id,
         Pending_Vehicles => This.Pending_Vehicles,
         Found            => Boarded);
   end Ask_For_Vehicle;

   function Leave_Parking (This         : in out Garage.Object;
                           Passenger_Id : in     Agent.Agent_Id;
                           Vehicle_Id   :    out Agent.Agent_Id)
   return Boolean is
   begin
      -- Make vehicle go out of Leaving_Vehicles map
      if This.Leaving_Vehicles.Is_The_Driver (Passenger_Id) then
         This.Leaving_Vehicles.Leave (Passenger_Id, Vehicle_Id);
         return True;
      end if;
      return False;
   end Leave_Parking;

   function Is_A_Driver (This         : in out Garage.Object;
                         Traveller_Id : in     Agent.Agent_Id)
   return Boolean is (This.Pending_Vehicles.Is_A_Driver (Traveller_Id));

   procedure Unboard_Vehicle (
      This             : in out Garage.Object;
      Vehicle_Id       : in     Agent.Agent_Id;
      Driver_Id        : in     Agent.Agent_Id) is
   begin
      This.Parked_Vehicles.Unboard_Vehicle (
         This.Pending_Vehicles, Vehicle_Id, Driver_Id);
   end Unboard_Vehicle;

   function Book_Parking (This       : in out Garage.Object;
                          Vehicle_Id : in     Agent.Agent_Id)
   return Boolean is
      Booked : Boolean := False;
   begin
      This.Parked_Vehicles.Book (
         This.Leaving_Vehicles, This.Pending_Vehicles, Vehicle_Id, Booked);
      return Booked;
   end Book_Parking;

   procedure Set_Host_Id (This    : in out Garage.Object;
                          Host_Id : in     Infra_Id) is
   begin
      This.Parked_Vehicles.Set_Host_Id (Host_Id);
   end Set_Host_Id;

   function Get_Host_Id (This    : in out Garage.Object)
   return Infra_Id is (This.Parked_Vehicles.Get_Host_Id);

   procedure Set_Size (This : in out Garage.Object;
                       Size : in     Natural)
   is
   begin
      This.Parked_Vehicles.all.Set_Size (Size);
   end Set_Size;

   overriding
   function Dump (This : Garage.Object) return G_JSON.JSON_Value
   is
      JSON         : G_JSON.JSON_Value := G_JSON.Create_Object;
      Parked_JSON  : G_JSON.JSON_Array;
      Leaving_JSON : G_JSON.JSON_Array;
      Pending_JSON : G_JSON.JSON_Array;
   begin
      JSON.Set_Field (Capacity_Field, Integer (This.Parked_Vehicles.Get_Size));

   -- Dump parked vehicles
      Parked_JSON := This.Parked_Vehicles.Dump;
      JSON.Set_Field (Parked_Field, Parked_JSON);

   -- Dump leaving vehicles
      Leaving_JSON := This.Leaving_Vehicles.Dump;
      JSON.Set_Field (Leaving_Field, Leaving_JSON);

   -- Dump pending vehicles
      Pending_JSON := This.Pending_Vehicles.Dump;
      JSON.Set_Field (Pending_Field, Pending_JSON);

      return JSON;
   end Dump;

----------------------
-- LEAVING VEHICLES
----------------------

   protected body Leaving_Vehicles is

      procedure Add_Vehicle (Vehicle_Id  : in Agent.Agent_Id;
                             Driver_Id   : in Agent.Agent_Id) is
      begin
         Vehicles.Include (Vehicle_Id);
         Drivers.Include  (Driver_Id, Vehicle_Id);
      end Add_Vehicle;

      procedure Board_Vehicle (
         Vehicle_Id   :    out Agent.Agent_Id;
         Traveller_Id : in     Agent.Agent_Id;
         Destination  : in     Slice.Map;
         Boarded      :    out Boolean)
      is
         Vehicle_Destination : Slice.Map;
      begin
         Boarded := False;

         for Vehicle of Vehicles loop
            Vehicle_Id := Vehicle;
            Vehicle_Destination := T_Utils.Get_Travel_Destination (Vehicle);

         -- Check if destination is the same as the one passed as parameter
            if Destination = Vehicle_Destination
         -- and whether there is room
            and not PC_Utils.Is_Carrier_Full (Vehicle_Id) then
               PC_Utils.Board (Vehicle_Id, Traveller_Id, Boarded);
               return;
            end if;

         -- Check if vehicle's travel contains traveller's destination
            if T_Utils.Does_Travel_Contain_Steps (Vehicle_Id, Destination)
            and not PC_Utils.Is_Carrier_Full (Vehicle_Id) then
               PC_Utils.Board (Vehicle_Id, Traveller_Id, Boarded);
               return;
            end if;

         end loop;

      end Board_Vehicle;

      function Is_The_Driver (Traveller_Id : in Agent.Agent_Id)
      return Boolean is (Drivers.Contains (Traveller_Id));

      procedure Leave (Traveller_Id : in     Agent.Agent_Id;
                       Vehicle_Id   :    out Agent.Agent_Id) is
      begin
         Vehicle_Id := Drivers.Element (Traveller_Id);
         Drivers.Delete (Traveller_Id);
         Vehicles.Delete (Vehicle_Id);
      end Leave;

      function Count return Natural
      is (Natural (Vehicles.Length));

      function Dump return G_JSON.JSON_Array
      is
         Item_JSON  : G_JSON.JSON_Value;
         Items_JSON : G_JSON.JSON_Array := G_JSON.Empty_Array;
      begin
         for Item in Drivers.Iterate loop
            Item_JSON := G_JSON.Create_Object;
            Item_JSON.Set_Field (
               Vehicle_Field,
               Agent_Id_To_Agent_Id_Map.Element (Item));
            Item_JSON.Set_Field (
               Driver_Field,
               Agent_Id_To_Agent_Id_Map.Key (Item));
            G_JSON.Append (Items_JSON, Item_JSON);
         end loop;
         return Items_JSON;
      end Dump;

      procedure Set_Traveller_Utils (
         T_Utils_Arg : access Traveller_Utils.Object'Class := null) is
      begin
         T_Utils := T_Utils_Arg;
      end Set_Traveller_Utils;

      procedure Set_People_Carrier_Utils (
         PC_Utils_Arg : access People_Carrier_Utils.Object'Class := null) is
      begin
         PC_Utils := PC_Utils_Arg;
      end Set_People_Carrier_Utils;

   end Leaving_Vehicles;

---------------------
-- PARKED VEHICLES
---------------------

   protected body Parked_Vehicles is

      procedure Add_Vehicle (Vehicle_Id : in     Agent.Agent_Id;
                             Parked     :    out Boolean) is
      begin

         if Vehicles.Contains (Vehicle_Id) then
            Parked := True;
            return;
         end if;

         Parked := False;
         if Natural (Vehicles.Length) < Size then
            Vehicles.Include (Vehicle_Id);
            Parked := True;
         end if;

      end Add_Vehicle;

      procedure Board_Vehicle (
         Vehicle_Id       :    out Agent.Agent_Id;
         Traveller_Id     : in     Agent.Agent_Id;
         Pending_Vehicles : access Garage.Pending_Vehicles;
         Found            :    out Boolean)
      is
         First_Vehicle  : Agent_Id_Set.Cursor;
         Source         : Slice.Map
            := T_Utils.Get_Travel_Source (Traveller_Id);
         Destination    : Slice.Map
            := T_Utils.Get_Travel_Destination (Traveller_Id);
      begin
         First_Vehicle := Vehicles.First;

         if First_Vehicle = Agent_Id_Set.No_Element then
            Found := False;
            return;
         end if;

         Vehicle_Id := Vehicles.First_Element;
         Found := True;
         T_Utils.Set_Travel(
            Vehicle_Id,
            Travel_Pkg.Create (
               Route_Source      => Source,
               Route_Destination => Destination,
               Travel_State      => Travel_Pkg.Travel_Planning.Get_Instance,
               Traveller_Id      => Vehicle_Id)
         );
         Pending_Vehicles.Put_Vehicle (Vehicle_Id, Traveller_Id);
         Vehicles.Delete_First;
      end Board_Vehicle;

      procedure Unboard_Vehicle (
         Pending_Vehicles : access Garage.Pending_Vehicles;
         Vehicle_Id       : in     Agent.Agent_Id;
         Driver_Id        : in     Agent.Agent_Id)
      is
         Left_Vehicle : Agent.Agent_Id;
         Parked_Again : Boolean;
      begin
         Add_Vehicle (Vehicle_Id, Parked_Again);
         Pending_Vehicles.Remove_Vehicle (Vehicle_Id, Driver_Id);
      end Unboard_Vehicle;

      procedure Book (
         Leaving_Vehicles : access Garage.Leaving_Vehicles;
         Pending_Vehicles : access Garage.Pending_Vehicles;
         Vehicle_Id       : in     Agent.Agent_Id;
         Booked           :    out Boolean)
      is
         Room : Natural := Size - Leaving_Vehicles.Count;
      begin
         Pending_Vehicles.Add_Vehicle (Vehicle_Id, Room, Booked);
      end Book;

      procedure Set_Host_Id (New_Host_Id : in Infra_Id) is
      begin
         Host_Id := New_Host_Id;
      end Set_Host_Id;

      function Get_Host_Id
      return Infra_Id is (Host_Id);

      procedure Set_Size (New_Size : in Natural) is
      begin
         Size := New_Size;
      end Set_Size;

      function Get_Size return Natural is (Size);

      function Contains (Vehicle_Id : in Agent.Agent_Id) return Boolean
      is (Vehicles.Contains (Vehicle_Id));

      procedure Set_Traveller_Utils (
         T_Utils_Arg : access Traveller_Utils.Object'Class := null) is
      begin
         T_Utils := T_Utils_Arg;
      end Set_Traveller_Utils;

      function Dump return G_JSON.JSON_Array
      is
         Vehicle_JSON  : G_JSON.JSON_Value;
         Vehicles_JSON : G_JSON.JSON_Array := G_JSON.Empty_Array;
      begin
         for Vehicle of Vehicles loop
            Vehicle_JSON := G_JSON.Create (Vehicle);
            G_JSON.Append (Vehicles_JSON, Vehicle_JSON);
         end loop;
         return Vehicles_JSON;
      end Dump;

   end Parked_Vehicles;

---------------------
-- PENDING VEHICLES
---------------------

   protected body Pending_Vehicles is

      procedure Add_Vehicle (Vehicle_Id : in     Agent.Agent_Id;
                             Room       : in     Natural;
                             Booked     :    out Boolean) is
      begin
         if Natural (Vehicles.Length) < Room then
            Vehicles.Include (Vehicle_Id);
            Booked := True;
         else
            Booked := False;
         end if;
      end Add_Vehicle;

      procedure Put_Vehicle (Vehicle_Id : in Agent.Agent_Id) is
      begin
         Vehicles.Include (Vehicle_Id);
      end Put_Vehicle;

      procedure Put_Vehicle (
         Vehicle_Id : in Agent.Agent_Id;
         Driver_Id  : in Agent.Agent_Id) is
      begin
         Vehicles.Include (Vehicle_Id);
         Drivers.Include (Driver_Id);
      end Put_Vehicle;

      procedure Remove_Vehicle (Vehicle_Id : in Agent.Agent_Id)
      is
      begin
         if Vehicles.Contains (Vehicle_Id) then
            Vehicles.Exclude (Vehicle_Id);
         end if;
      end Remove_Vehicle;

      procedure Remove_Vehicle (
         Vehicle_Id : in Agent.Agent_Id;
         Driver_Id  : in Agent.Agent_Id)
      is
      begin
         Remove_Vehicle (Vehicle_Id);
         if Drivers.Contains (Driver_Id) then
            Drivers.Exclude (Driver_Id);
         end if;
      end Remove_Vehicle;

      function Is_A_Driver (Traveller_Id : in Agent.Agent_Id)
      return Boolean is (Drivers.Contains (Traveller_Id));

      function Dump return G_JSON.JSON_Array
      is
         Vehicle_JSON  : G_JSON.JSON_Value;
         Vehicles_JSON : G_JSON.JSON_Array := G_JSON.Empty_Array;
      begin
         for Vehicle of Vehicles loop
            Vehicle_JSON := G_JSON.Create (Vehicle);
            G_JSON.Append (Vehicles_JSON, Vehicle_JSON);
         end loop;
         return Vehicles_JSON;
      end Dump;

   end Pending_Vehicles;

end Reactive.Infrastructure.Building.Parking_Manager.Garage;
