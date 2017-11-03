-- core
with Ada.Strings.Unbounded;

with Reactive.Infrastructure.Building.Host.Facility;
with Reactive.Infrastructure.Building.Parking_Manager.Garage;

with Reactive.Infrastructure.Build.Exceptions;
use Reactive.Infrastructure.Build.Exceptions;

package body Reactive.Infrastructure.Build.Host_Builder is

   package SU            renames Ada.Strings.Unbounded;

   function Create (
      District         : access Reactive.District.Object'Class := null)
   return Host_Builder.Reference
   is
      Instance : Host_Builder.Reference := new Host_Builder.Object;
   begin
      Instance.District := District;
      if Instance.District = null then
         Instance.District := Reactive.District.Get_Instance;
      end if;
      return Instance;
   end Create;

   not overriding
   procedure With_Parking_Manager (This : in out Host_Builder.Object;
                                   JSON : in     G_JSON.JSON_Value)
   is
      Leaving_Vehicles : G_JSON.JSON_Array;
   begin
      This.Parking_Manager :=
         Parking_Manager_Pkg.Reference (Parking_Manager_Pkg.Garage.Create);

      if not JSON.Has_Field (Capacity_Field) then
         Raise_Missing_Field_For_Host (Capacity_Field);
      end if;

      This.Parking_Manager.all.Set_Size (JSON.Get (Capacity_Field));

      if not JSON.Has_Field (Parked_Field) then
         Raise_Missing_Field_For_Host (Parked_Field);
      end if;
      declare
         Parked_Vehicles : G_JSON.JSON_Value;
      begin
         Parked_Vehicles := JSON.Get (Parked_Field);
         This.Set_Parked_Vehicles (Parked_Vehicles.Get);
      end;

      if not JSON.Has_Field (Leaving_Field) then
         Raise_Missing_Field_For_Host (Leaving_Field);
      end if;
      This.Set_Leaving_Vehicles (JSON.Get (Leaving_Field));

      if not JSON.Has_Field (Pending_Field) then
         Raise_Missing_Field_For_Host (Pending_Field);
      end if;
      This.Set_Pending_Vehicles (JSON.Get (Pending_Field));
   end With_Parking_Manager;

   not overriding
   function Get_Host (This : in out Host_Builder.Object;
                      JSON : in     G_JSON.JSON_Value)
   return Infra_Id
   is
      Host             : aliased Host_Pkg.Facility.Reference;
      Host_Id_Int      : Integer;
      Host_Id          : Infra_Id;
      Guests_JSON      : G_JSON.JSON_Array;
      Guest_JSON       : G_JSON.JSON_Value;
      Guest_SU         : SU.Unbounded_String;
      Guest            : Agent.Agent_Id;
   begin
   -- ID
      if not JSON.Has_Field (Id_Field) then
         Raise_Missing_Field_For_Host (Id_Field);
      end if;
      Host_Id_Int := JSON.Get (Id_Field);
      Host_Id     := Infra_Id (Host_Id_Int);
      Host := Host_Pkg.Facility
              .Create (Host_Id, This.Parking_Manager);

   -- GUESTS
      if not JSON.Has_Field (Guests_Field) then
         Raise_Missing_Field_For_Host (Guests_Field);
      end if;
      Guests_JSON := JSON.Get (Guests_Field);
      for I in 1 .. G_JSON.Length (Guests_JSON) loop
         Guest_JSON := G_JSON.Get (Guests_JSON, I);
         Guest_SU   := SU.To_Unbounded_String (Guest_JSON.Write);
         Guest      := Guest_SU;
         Host.Put_Stopping_Traveller (Guest);
      end loop;

      declare
         Added : Boolean := False;
      begin
         This.District.Add_Host (Host.all, Added);
      end;

      return Host_Id;
   end Get_Host;

--private

   procedure Set_Parked_Vehicles (
      This        : in out Host_Builder.Object;
      Parked_JSON : in     G_JSON.JSON_Array)
   is
      Vehicle_JSON : G_JSON.JSON_Value;
      Vehicle      : Agent.Agent_Id;
   begin
      for I in 1 .. G_JSON.Length (Parked_JSON) loop
         Vehicle_JSON := G_JSON.Get (Parked_JSON, I);
         Vehicle   :=
            Agent.Agent_Id (SU.To_Unbounded_String (Vehicle_JSON.Write));
      -- add vehicles to parking
         This.Parking_Manager.all.Park_Vehicle (Vehicle);
      end loop;
   end Set_Parked_Vehicles;

   procedure Set_Leaving_Vehicles (
      This        : in out Host_Builder.Object;
      Leaving_JSON : in     G_JSON.JSON_Array)
   is
      Vehicle_And_Driver_JSON : G_JSON.JSON_Value;
      Vehicle_SU              : SU.Unbounded_String;
      Vehicle                 : Agent.Agent_Id;
      Driver_SU               : SU.Unbounded_String;
      Driver                  : Agent.Agent_Id;
   begin
      for I in 1 .. G_JSON.Length (Leaving_JSON) loop
         Vehicle_And_Driver_JSON := G_JSON.Get (Leaving_JSON, I);
         Vehicle_SU   := Vehicle_And_Driver_JSON.Get (Vehicle_Field);
         Vehicle      := Vehicle_SU;
         Driver_SU    := Vehicle_And_Driver_JSON.Get (Driver_Field);
         Driver       := Driver_SU;
      -- put leaving vehicle with driver in parking
         This.Parking_Manager.all.Put_Leaving_Vehicle (
            Vehicle_Id => Vehicle,
            Driver_Id  => Driver);
      end loop;
   end Set_Leaving_Vehicles;

   procedure Set_Pending_Vehicles (
      This         : in out Host_Builder.Object;
      Pending_JSON : in     G_JSON.JSON_Array)
   is
      Vehicle_JSON : G_JSON.JSON_Value;
      Vehicle      : Agent.Agent_Id;
   begin
      for I in 1 .. G_JSON.Length (Pending_JSON) loop
         Vehicle_JSON := G_JSON.Get (Pending_JSON, I);
         Vehicle   :=
            Agent.Agent_Id (SU.To_Unbounded_String (Vehicle_JSON.Write));
      -- put pending vehicles in parking
         This.Parking_Manager.Put_Pending_Vehicle (Vehicle);
      end loop;
   end Set_Pending_Vehicles;

end Reactive.Infrastructure.Build.Host_Builder;
