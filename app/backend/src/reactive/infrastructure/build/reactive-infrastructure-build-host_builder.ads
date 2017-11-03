-- core
with Ada.Finalization;
-- gnatcoll libs
with GNATCOLL.JSON;

with Active.Agent;

with Reactive.District;
with Reactive.Infrastructure.Building.Host;
with Reactive.Infrastructure.Building.Parking_Manager;

with Shared.Agent_Id_List;

package Reactive.Infrastructure.Build.Host_Builder is

-- libs
   package G_JSON renames GNATCOLL.JSON;
-- active
   package Agent  renames Active.Agent;
-- reactive
   package Parking_Manager_Pkg
      renames Reactive.Infrastructure.Building.Parking_Manager;
   package Host_Pkg
      renames Reactive.Infrastructure.Building.Host;
-- shared
   package Agent_Id_List renames Shared.Agent_Id_List;

   use Reactive.Infra_Id_Type;

   type Object is new Ada.Finalization.Controlled with private;
   type Reference is access all Object'Class;

   function Create (
      District : access Reactive.District.Object'Class := null)
   return Host_Builder.Reference;

   not overriding
   procedure With_Parking_Manager (This : in out Host_Builder.Object;
                                   JSON : in     G_JSON.JSON_Value);

   not overriding
   function Get_Host (This : in out Host_Builder.Object;
                      JSON : in     G_JSON.JSON_Value)
   return Infra_Id;

private
   type Object is new Ada.Finalization.Controlled with record
      Parking_Manager : Parking_Manager_Pkg.Reference;
      District        : access Reactive.District.Object'Class := null;
   end record;

   procedure Set_Parked_Vehicles (
      This        : in out Host_Builder.Object;
      Parked_JSON : in     G_JSON.JSON_Array);

   procedure Set_Leaving_Vehicles (
      This         : in out Host_Builder.Object;
      Leaving_JSON : in     G_JSON.JSON_Array);

   procedure Set_Pending_Vehicles (
      This         : in out Host_Builder.Object;
      Pending_JSON : in     G_JSON.JSON_Array);

   Id_Field       : constant String := Host_Pkg.Id_Field;
   Guests_Field   : constant String := Host_Pkg.Guests_Field;
   Capacity_Field : constant String := Parking_Manager_Pkg.Capacity_Field;
   Leaving_Field  : constant String := Parking_Manager_Pkg.Leaving_Field;
   Pending_Field  : constant String := Parking_Manager_Pkg.Pending_Field;
   Parked_Field   : constant String := Parking_Manager_Pkg.Parked_Field;
   Vehicle_Field  : constant String := Parking_Manager_Pkg.Vehicle_Field;
   Driver_Field   : constant String := Parking_Manager_Pkg.Driver_Field;

end Reactive.Infrastructure.Build.Host_Builder;
