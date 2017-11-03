------------------------------------------------------------------------------
-- @author  <gianmarco.midena@gmail.com>
-- @author  <stefanomunari.sm@gmail.com>
-- @author  <valle.sebastiano93@gmail.com>
-- @context application-backend::active-build-traveller-config_reader
-- @purpose Reads the configuration for creating a single traveller
-- @interface Set_Builder (Object, Builder):
--              sets the builder which will be used during the creation process
--            Read (Object, JSON) -> Agent_Id
--              starts the creation of a traveller and returns its id
--              WARNING: Call this method *after* having called all the With_*
--                       operations for a given traveller. Any invocation made
--                       after the call of Get_Result will have no effect
-- @dependencies application-backend::active-agent
--               application-backend::active-build-traveller-builder
--               application-backend::active-traveller
--               application-backend::active-traveller-pedestrian
--               application-backend::active-traveller-vehicle
--               application-backend::active-traveller-vehicle-bus
--               application-backend::active-traveller-vehicle-private_motor_vehicle
-- @details all JSON fields are inherited/copied by the concrete traveller
--          packages
------------------------------------------------------------------------------

-- core
with Ada.Finalization;
-- gnatcoll libs
with GNATCOLL.JSON;

with Active.Agent;
with Active.Build.Traveller.Builder;
with Active.Traveller;
with Active.Traveller.Pedestrian;
with Active.Traveller.Vehicle;
with Active.Traveller.Vehicle.Bus;
with Active.Traveller.Vehicle.Private_Motor_Vehicle;

package Active.Build.Traveller.Config_Reader is

   package G_JSON renames GNATCOLL.JSON;
   package Agent  renames Active.Agent;
   package Traveller_Pkg  renames Active.Traveller;
   package Pedestrian_Pkg renames Active.Traveller.Pedestrian;
   package Vehicle_Pkg    renames Active.Traveller.Vehicle;
   package Bus_Pkg        renames Active.Traveller.Vehicle.Bus;
   package PVT_Pkg
      renames Active.Traveller.Vehicle.Private_Motor_Vehicle;
   use Agent;

   type Object is new Ada.Finalization.Controlled with private;
   type Reference is access all Object'Class;

   not overriding
   procedure Set_Builder (
      This        :    out Build.Traveller.Config_Reader.Object;
      Builder_Ref : not null Builder.Reference);

   not overriding
   function Read (
      This           : in out Traveller.Config_Reader.Object;
      Traveller_Json : in     G_JSON.JSON_Value)
   return Agent.Agent_Id;

   Id_Field               : constant String := Traveller_Pkg.Id_Field;
   Type_Field             : constant String := Traveller_Pkg.Type_Field;
   Source_Field           : constant String := Traveller_Pkg.Source_Field;
   Destination_Field      : constant String := Traveller_Pkg.Destination_Field;
   Residual_Travel_Field  : constant String :=
      Traveller_Pkg.Residual_Route_Field;
   Travel_State_Field     : constant String := Traveller_Pkg.Travel_State_Field;
   Current_Speed_Field    : constant String :=
      Traveller_Pkg.Current_Speed_Field;
   Max_Speed_Field        : constant String :=
      Traveller_Pkg.Maximum_Speed_Field;
   Current_Position_Field : constant String :=
      Traveller_Pkg.Current_Position_Field;
   Passengers_Field       : constant String := Vehicle_Pkg.Passengers_Field;
   Max_Passengers_Field   : constant String := Vehicle_Pkg.Max_Passengers_Field;
   PVT_Type_Field         : constant String := PVT_Pkg.PVT_Type_Field;
   Bus_Stops_Field        : constant String := Bus_Pkg.Bus_Stops_Field;
   Route_Stops_Field      : constant String := Bus_Pkg.Route_Stops_Field;
   Is_Waiting_Field       : constant String := Pedestrian_Pkg.Is_Waiting_Field;

private
   type Object is new Ada.Finalization.Controlled with record
      Builder_Ref : Builder.Reference;
   end record;

end Active.Build.Traveller.Config_Reader;
