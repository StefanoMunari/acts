------------------------------------------------------------------------------
-- @author  <gianmarco.midena@gmail.com>
-- @author  <stefanomunari.sm@gmail.com>
-- @author  <valle.sebastiano93@gmail.com>
-- @context application-backend::active-traveller-vehicle-bus
-- @purpose Represents a bus
-- @interface Travel (Object) -> Boolean:
--              does not inherit the implementation directly from Vehicle,
--              rather from Traveller
--            On_Bus_Stop (Object) -> Boolean:
--              boards and lands passengers
-- @dependencies application-backend::active-agent
--               application-backend::active-bus_service
--               application-backend::active-space_master
--               application-backend::active-traveller-pedestrian-utils
--               application-backend::passive-road_sign-bus_stop
--               application-backend::reactive-infrastructure-lane-utils
--               application-backend::reactive-infrastructure-way-utils
--               application-backend::reactive-infrastructure-stretch-decoration-stretch_sign_decorator-utils
-- @details Implementation of Bus. Most of the methods are not listed in
--          the @interface API since they're self-explanatory
------------------------------------------------------------------------------

with Active.Agent;
with Active.Bus_Service;
with Active.Space_Master;
with Active.Traveller.Pedestrian.Utils;

with Interface_Layer.Remote.Stub;
with Interface_Layer.Wrappers.Application.Abstract_Factory;

with Passive.Road_Sign.Bus_Stop;

with Reactive.Infrastructure.Lane.Utils;
with Reactive.Infrastructure.Way.Utils;
with Reactive.Infrastructure.Stretch.Decoration.Stretch_Sign_Decorator.Utils;

package Active.Traveller.Vehicle.Bus is

   package Agent           renames Active.Agent;
   package Space_Master    renames Active.Space_Master;
   package Vehicle         renames Active.Traveller.Vehicle;
   package Pedestrian      renames Active.Traveller.Pedestrian;
   package Stub_Pkg        renames Interface_Layer.Remote.Stub;
   package App_Wrapper_Pkg renames Interface_Layer.Wrappers.Application;
   package Road_Sign       renames Passive.Road_Sign;
   package Lane            renames Reactive.Infrastructure.Lane;
   package Way             renames Reactive.Infrastructure.Way;
   package Stretch_Sign_Decorator renames
      Reactive.Infrastructure.Stretch.Decoration.Stretch_Sign_Decorator;

   type Object is
     new Vehicle.Object
     and Bus_Service.Object
   with private;
   type Reference is access all Bus.Object'Class;

   not overriding
   function Create (
      Id             : in     Agent.Agent_Id;
      Maximum_Speed  : in     Natural;
      Max_Passengers : in     Natural;
      Travel_Ref     : access Active.Travel.Object'Class;
      Bus_Stops      : in     Infra_Id_List.List;
      Route_Stops    : in     Infra_Id_List.List;
      Space_Master_Ref     : access Space_Master.Object'Class := null;
      Infrastructure_Utils : access Infrastructure.Utils.Object'Class := null;
      Stretch_Utils        : access Stretch.Utils.Object'Class := null;
      Street_Utils         : access Street.Utils.Object'Class := null;
      Host_Utils           : access Host.Utils.Object'Class := null;
      Traveller_Utils      : access Traveller.Utils.Object'Class := null;
      Lane_Utils           : access Lane.Utils.Object'Class := null;
      Way_Utils            : access Way.Utils.Object'Class := null;
      Intersection_Utils   : access Intersection.Utils.Object'Class := null;
      Pedestrian_Utils     : access Pedestrian.Utils.Object'Class := null;
      SSD_Utils            : -- Stretch_Sign_Decorator
         access Stretch_Sign_Decorator.Utils.Object'Class := null;
      Stub                 :
         access Stub_Pkg.Object'Class := null;
      App_Wrapper_Factory  :
         access App_Wrapper_Pkg.Abstract_Factory.Object'Class:= null)
   return Bus.Reference;

   overriding
   procedure Travel (This : in out Bus.Object);

   overriding
   function Is_Affected_By_Traffic_Lights (This : in Bus.Object)
   return Boolean;

   overriding
   function Get_Size (This : in Bus.Object) return Natural;

   overriding
   procedure On_Bus_Stop (This : in out Bus.Object);

   not overriding
   function Get_Route_Stops (This : in Bus.Object) return Infra_Id_List.List;

   overriding
   function Dump (This : in Bus.Object)
   return G_JSON.JSON_Value;

-- JSON FIELDS CONSTANTS
   function Bus_Stops_Field   return String is ("busStops");
   function Route_Stops_Field return String is ("routeStops");

private
   type Object is
     new Vehicle.Object
     and Bus_Service.Object
   with record
   -- Bus_Stops: footway stretches
      Bus_Stops   : Infra_Id_List.List := Infra_Id_List.Empty_List;
   -- Route_Stops: roadway stretches
      Route_Stops : Infra_Id_List.List := Infra_Id_List.Empty_List;
      Space_Master_Ref    : access Space_Master.Object'Class := null;
      Lane_Utils          : access Lane.Utils.Object'Class := null;
      Way_Utils           : access Way.Utils.Object'Class := null;
      Pedestrian_Utils    : access Pedestrian.Utils.Object'Class := null;
      SSD_Utils           :
         access Stretch_Sign_Decorator.Utils.Object'Class := null;
      Stub                :
         access Stub_Pkg.Object'Class := null;
      App_Wrapper_Factory :
         access App_Wrapper_Pkg.Abstract_Factory.Object'Class:= null;
   end record;

   function Get_Landing_Passengers (
      This            : in out Bus.Object;
      Landing_Stretch : in     Infra_Id)
   return Agent_Id_List.List;

   function Get_Boarding_Passengers (
      This         : in out Bus.Object;
      Bus_Stop_Ref : in     Road_Sign.Bus_Stop.Reference)
   return Agent_Id_List.List;

   function Land_Passengers (
      This            : in out Bus.Object;
      Landing_Stretch : in     Infra_Id;
      Passengers      : in     Agent_Id_List.List)
   return Agent_Id_List.List;

   function Board_Passengers (
      This            : in out Bus.Object;
      Landing_Stretch : in     Infra_Id;
      Passengers      : in     Agent_Id_List.List;
      Bus_Stop_Ref    :        Road_Sign.Bus_Stop.Reference)
   return Agent_Id_List.List;

end Active.Traveller.Vehicle.Bus;
