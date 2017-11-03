------------------------------------------------------------------------------
-- @author  <gianmarco.midena@gmail.com>
-- @author  <stefanomunari.sm@gmail.com>
-- @author  <valle.sebastiano93@gmail.com>
-- @context application-backend::active-build-traveller-builder
-- @purpose Builds a traveller, given some details of its state
-- @interface With_* (Object, ..):
--              stores one or more values for the traveller costruction
--            Get_Result
--              returns the traveller
--              WARNING: Call this method *after* having called all the With_*
--                       operations for a given traveller. Any invocation made
--                       after the call of Get_Result will have no effect
--              WARNING: Calling this method will cause the traveller to be
--                       added to the district
-- @dependencies application-backend::active-agent
--               application-backend::active-travel-travel_state
--               application-backend::active-traveller
--               application-backend::active-traveller-vehicle-private_motor_vehicle
--               application-backend::reactive
--               application-backend::shared-agent_id_list
--               application-backend::shared-infra_id_list
--               application-backend::shared-slice
-- @details Remember to instantiate a distinct builder for *each* traveller you
--          are creating
------------------------------------------------------------------------------

-- core
with Ada.Finalization;
-- gnatcoll libs
with GNATCOLL.JSON;

with Active.Agent;
with Active.Travel.Travel_State;
with Active.Traveller;
with Active.Traveller.Vehicle.Private_Motor_Vehicle;

with Reactive;

with Shared.Agent_Id_List;
with Shared.Infra_Id_List;
with Shared.Slice;

package Active.Build.Traveller.Builder is

   package G_JSON        renames GNATCOLL.JSON;
-- active
   package Agent            renames Active.Agent;
   package Travel_State_Pkg renames Active.Travel.Travel_State;
   package Traveller_Pkg    renames Active.Traveller;
   package PVT_Pkg          renames
      Active.Traveller.Vehicle.Private_Motor_Vehicle;
-- shared
   package Agent_Id_List renames Shared.Agent_Id_List;
   package Infra_Id_List renames Shared.Infra_Id_List;
   package Slice renames Shared.Slice;
-- use
   use Agent;
   use Reactive.Infra_Id_Type;
   use PVT_Pkg;

   type Object is new Ada.Finalization.Controlled with private;
   type Reference is access all Object'Class;

   not overriding
   function With_Id (
      This : in out Traveller.Builder.Object;
      Id   : in     Agent_Id)
   return Builder.Reference;

   not overriding
   function With_Src (
      This       : in out Traveller.Builder.Object;
      Slice_JSON : in     G_JSON.JSON_Value)
   return Builder.Reference;

   not overriding
   function With_Dst (
      This       : in out Traveller.Builder.Object;
      Slice_JSON : in     G_JSON.JSON_Value)
   return Builder.Reference;

   not overriding
   function With_Residual_Travel (
      This            : in out Traveller.Builder.Object;
      Residual_Travel : in     G_JSON.JSON_Array)
   return Builder.Reference;

   not overriding
   function With_Travel_State (
      This         : in out Traveller.Builder.Object;
      Travel_State : in     SU.Unbounded_String)
   return Builder.Reference;

   not overriding
   function With_Current_Speed (
      This  : in out Traveller.Builder.Object;
      Speed : in     Natural)
   return Builder.Reference;

   not overriding
   function With_Max_Speed (
      This  : in out Traveller.Builder.Object;
      Speed : in     Natural)
   return Builder.Reference;

   not overriding
   function With_Current_Position (
      This     : in out Traveller.Builder.Object;
      Position : in     Infra_Id)
   return Builder.Reference;

   not overriding
   function With_Passengers (
      This       : in out Traveller.Builder.Object;
      Passengers : in     G_JSON.JSON_Array)
   return Builder.Reference;

   not overriding
   function With_Max_Passengers (
      This           : in out Traveller.Builder.Object;
      Max_Passengers : in     Natural)
   return Builder.Reference;

   not overriding
   function With_Pvt_Type (
      This     : in out Traveller.Builder.Object;
      PVT_Type : in     SU.Unbounded_String)
   return Builder.Reference;

   not overriding
   function With_Bus_Stops (
      This  : in out Traveller.Builder.Object;
      Stops : in     G_JSON.JSON_Array)
   return Builder.Reference;

   not overriding
   function With_Route_Stops (
      This  : in out Traveller.Builder.Object;
      Stops : in     G_JSON.JSON_Array)
   return Builder.Reference;

   not overriding
   function With_Is_Waiting (
      This       : in out Traveller.Builder.Object;
      Is_Waiting : in     Boolean)
   return Builder.Reference;

   function Get_Result (
      This   : in out Traveller.Builder.Object;
      T_Type : in     SU.Unbounded_String)
   return Agent_Id;

private
   type Object is new Ada.Finalization.Controlled with record
      Id                 : Agent_Id;
      Travel_Source      : Slice.Map;
      Travel_Destination : Slice.Map;
      Residual_Travel    : Infra_Id_List.List;
      Travel_State       : Travel_State_Pkg.Reference;
      Current_Speed      : Natural;
      Max_Speed          : Natural;
      Current_Position   : Infra_Id;
      Passengers         : Agent_Id_List.List;
      Max_Passengers     : Natural;
      PVT_Type           : Private_Motor_Vehicle_Type;
      Bus_Stops          : Infra_Id_List.List;
      Route_Stops        : Infra_Id_List.List;
      Is_Waiting         : Boolean;
   end record;

   function Get_Stretches_For (
      Slice_JSON : in G_JSON.JSON_Value;
      Type_Field : in String)
   return Infra_Id_List.List;

   procedure Put_Passengers (
      Traveller  : Traveller_Pkg.Reference;
      Passengers : Agent_Id_List.List);

end Active.Build.Traveller.Builder;
