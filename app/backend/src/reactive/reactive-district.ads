------------------------------------------------------------------------------
-- @author  <gianmarco.midena@gmail.com>
-- @author  <stefanomunari.sm@gmail.com>
-- @author  <valle.sebastiano93@gmail.com>
-- @context application-backend::active-travel
-- @purpose Represents a travel of a given traveller
-- @interface Contains_* (Object, Identifier) -> Boolean:
--              true iff the entity is in this district
--            Find_* (Object, Identifier) -> Reference:
--              returns a reference to a given entity
--            Add_* (Object, Reference):
--              adds an entity to the district
-- @dependencies application-backend::active-agent
--               application-backend::active-traffic_light
--               application-backend::active-traveller
--               application-backend::active-traveller-utils
--               application-backend::interface_layer-remote-sub
--               application-backend::interface_layer-wrappers-application-abstract_factory
--               application-backend::reactive-associable
--               application-backend::reactive-infrastructure
--               application-backend::reactive-infrastructure-building-host
--               application-backend::reactive-infrastructure-intersection
--               application-backend::reactive-infrastructure-street
--               application-backend::reactive-infrastructure-street_related_infrastructure
--               application-backend::reactive-infrastructure-way
--               application-backend::reactive-infrastructure-way-bikeway
--               application-backend::reactive-infrastructure-way-footway
--               application-backend::reactive-infrastructure-way-roadway
--               application-backend::reactive-infrastructure-lane
--               application-backend::reactive-infrastructure-stretch
--               application-backend::reactive-infrastructure_registry
--               application-backend::reactive-host_registry
--               application-backend::reactive-traffic_light_registry
--               application-backend::reactive-traveller_registry
--               application-backend::reactive-treadable
--               application-backend::shared-shared_references_street
--               application-backend::shared-slice
-- @details This class mostly forwards calls to the intended objects.
------------------------------------------------------------------------------

with Ada.Environment_Variables;

with GNATCOLL.JSON;

with Active.Agent;
with Active.Traffic_Light;
with Active.Traveller;
with Active.Traveller.Utils;

with Interface_Layer.Remote.Stub;
with Interface_Layer.Wrappers.Application.Abstract_Factory;

with Reactive.Associable;
with Reactive.Infrastructure;
with Reactive.Infrastructure.Building.Host;
with Reactive.Infrastructure.Intersection;
with Reactive.Infrastructure.Street;
with Reactive.Infrastructure.Street_Related_Infrastructure;
with Reactive.Infrastructure.Way;
with Reactive.Infrastructure.Way.Bikeway;
with Reactive.Infrastructure.Way.Footway;
with Reactive.Infrastructure.Way.Roadway;
with Reactive.Infrastructure.Lane;
with Reactive.Infrastructure.Stretch;
with Reactive.Infrastructure_Registry;
with Reactive.Host_Registry;
with Reactive.Traffic_Light_Registry;
with Reactive.Traveller_Registry;
with Reactive.Treadable;

with Shared.Shared_References_Street;

package Reactive.District is

-- active
   package Agent renames Active.Agent;
   package Traffic_Light renames Active.Traffic_Light;
   package Traveller renames Active.Traveller;
-- interface layer
   package Remote_Stub renames Interface_Layer.Remote.Stub;
-- reactive
   package Host_Pkg       renames Reactive.Infrastructure.Building.Host;
   package Infrastructure renames Reactive.Infrastructure;
   package Intersection   renames Reactive.Infrastructure.Intersection;
   package Street         renames Reactive.Infrastructure.Street;
   package Street_Related_Infrastructure
      renames Reactive.Infrastructure.Street_Related_Infrastructure;
   package Way            renames Reactive.Infrastructure.Way;
   package Roadway        renames Reactive.Infrastructure.Way.Roadway;
   package Footway        renames Reactive.Infrastructure.Way.Footway;
   package Bikeway        renames Reactive.Infrastructure.Way.Bikeway;
   package Lane           renames Reactive.Infrastructure.Lane;
   package Stretch        renames Reactive.Infrastructure.Stretch;
-- shared
   package SR_Street_Pkg  renames Shared.Shared_References_Street;

   use Reactive.Infra_Id_Type;
   package EV renames Ada.Environment_Variables;
   package G_JSON renames GNATCOLL.JSON;

   type Object (<>) is new Associable.Object with private;
   type Reference is access all District.Object'Class;

   function Get_Instance (
      Infrastructure_Registry :
         access Reactive.Infrastructure_Registry.Object'Class := null;
      Traffic_Light_Registry :
         access Reactive.Traffic_Light_Registry.Object'Class := null;
      Traveller_Registry :
         access Reactive.Traveller_Registry.Object'Class := null;
      Host_Registry :
         access Reactive.Host_Registry.Object'Class := null;
      App_Wrapper_Factory :
         access Interface_Layer.Wrappers.Application.Abstract_Factory.Object'Class:= null;
      Stub : access Remote_Stub.Object'Class := null;
      Traveller_Utils :
         access Active.Traveller.Utils.Object'Class := null)
   return District.Reference;

   not overriding
   function Contains_Infrastructure (This              : in District.Object;
                                     Infrastructure_Id : in Infra_Id)
   return Boolean;

   not overriding
   function Contains_Treadable (This         : in District.Object;
                                Treadable_Id : in Infra_Id)
   return Boolean;

   not overriding
   function Contains_Host (This    : in District.Object;
                           Host_Id : in Infra_Id)
   return Boolean;

   not overriding
   function Contains_Traveller (This         : in District.Object;
                                Traveller_Id : in Agent.Agent_Id)
   return Boolean;

   not overriding
   function Contains_Traffic_Light (This             : in District.Object;
                                    Traffic_Light_Id : in Agent.Agent_Id)
   return Boolean;

   not overriding
   function Dump (This : in District.Object) return G_JSON.JSON_Value;

   not overriding
   function Dump_Travellers (This : in District.Object)
   return G_JSON.JSON_Value;

   not overriding
   function Dump_Traffic_Lights (This : in District.Object)
   return G_JSON.JSON_Value;

   not overriding
   function Find_Infrastructure_By_Id (This              : in District.Object;
                                       Infrastructure_Id : in Infra_Id)
   return Infrastructure.Reference;

   not overriding
   function Find_Treadable_By_Id (This         : in District.Object;
                                  Treadable_Id : in Infra_Id)
   return Treadable.Reference;

   not overriding
   function Find_Intersection_By_Id (This            : in District.Object;
                                     Intersection_Id : in Infra_Id)
   return Intersection.Reference;

   not overriding
   function Find_Street_By_Id (This      : in District.Object;
                               Street_Id : in Infra_Id)
   return Street.Reference;

   not overriding
   function Find_Way_By_Id (This   : in District.Object;
                            Way_Id : in Infra_Id)
   return Way.Reference;

   not overriding
   function Find_Roadway_By_Id (This       : in District.Object;
                                Roadway_Id : in Infra_Id)
   return Roadway.Reference;

   not overriding
   function Find_Footway_By_Id (This       : in District.Object;
                                Footway_Id : in Infra_Id)
   return Footway.Reference;

   not overriding
   function Find_Bikeway_By_Id (This       : in District.Object;
                                Bikeway_Id : in Infra_Id)
   return Bikeway.Reference;

   not overriding
   function Find_Lane_By_Id (This    : in District.Object;
                             Lane_Id : in Infra_Id)
   return Lane.Reference;

   not overriding
   function Find_Stretch_By_Id (This       : in District.Object;
                                Stretch_Id : in Infra_Id)
   return Stretch.Reference;

   not overriding
   function Find_Agent_By_Id (This     : in District.Object;
                              Agent_Id : in Agent.Agent_Id)
   return Agent.Reference;

   not overriding
   function Find_Traveller_By_Id (This : in District.Object;
                                  Traveller_Id : in Agent.Agent_Id)
   return Traveller.Reference;

   not overriding
   function Find_Traffic_Light_By_Id (This             : in District.Object;
                                      Traffic_Light_Id : in Agent.Agent_Id)
   return Traffic_Light.Reference;

   not overriding
   function Find_Host_By_Id (This    : in District.Object;
                             Host_Id : in Infra_Id)
   return Host_Pkg.Reference;

   not overriding
   procedure Add_Intersection (
      This           : in out District.Object;
      Infrastructure :
         aliased in out Reactive.Infrastructure.Intersection.Object'Class;
      Added          : out Boolean);

   not overriding
   procedure Add_Street (
      This      : in out District.Object;
      SR_Street : in out SR_Street_Pkg.Shared_Reference;
      Added     :    out Boolean);

   not overriding
   procedure Add_Roadway (
      This           : in out District.Object;
      Infrastructure :
         aliased in out Reactive.Infrastructure.Way.Roadway.Reference;
      Added          : out Boolean);

   not overriding
   procedure Add_Footway (
      This           : in out District.Object;
      Infrastructure :
         aliased in out Reactive.Infrastructure.Way.Footway.Reference;
      Added          : out Boolean);

   not overriding
   procedure Add_Bikeway (
      This           : in out District.Object;
      Infrastructure :
         aliased in out Reactive.Infrastructure.Way.Bikeway.Reference;
      Added          : out Boolean);

   not overriding
   procedure Add_Lane (
      This           : in out District.Object;
      Infrastructure :
         aliased in out Reactive.Infrastructure.Lane.Reference;
      Added          : out Boolean);

   not overriding
   procedure Add_Stretch (
      This           : in out District.Object;
      Infrastructure :
         aliased in out Reactive.Infrastructure.Stretch.Reference;
      Added          : out Boolean);

   not overriding
   procedure Add_Traveller (
      This      : in out District.Object;
      Traveller : in out Active.Traveller.Reference;
      Added     : out Boolean);

   not overriding
   procedure Add_Traffic_Light (
      This          :         in out District.Object;
      Traffic_Light : aliased in out Active.Traffic_Light.Object'Class;
      Added         :            out Boolean);

   not overriding
   procedure Add_Host (
      This     :         in out District.Object;
      Host_Ref : aliased in out Host_Pkg.Object'Class;
      Added    :            out Boolean);

   not overriding
   procedure Remove_Traveller (
      This         : in out District.Object;
      Traveller_Id : in     Agent.Agent_Id;
      Removed      :    out Boolean);

   not overriding
   procedure Schedule (
      This      : in out District.Object;
      Traveller : aliased in out Active.Traveller.Reference);

   not overriding
   procedure Schedule (
      This          : in out District.Object;
      Traffic_Light : aliased in out Active.Traffic_Light.Reference;
      Offset        : Natural);

   overriding
   procedure Try_To_Tread_Infrastructure (
      This         : in out District.Object;
      Traveller_Id : in     Agent.Agent_Id;
      Treadable_Id : in     Infra_Id;
      Advanced     :    out Boolean);

private
   type Object is new Associable.Object with record
      Infrastructure_Registry :
         access Reactive.Infrastructure_Registry.Object'Class;
      Traveller_Registry :
         access Reactive.Traveller_Registry.Object'Class;
      Traffic_Light_Registry :
         access Reactive.Traffic_Light_Registry.Object'Class;
      Host_Registry :
         access Reactive.Host_Registry.Object'Class;
      App_Wrapper_Factory :
         access Interface_Layer.Wrappers.Application.Abstract_Factory.Object'Class;
      Stub :
         access Remote_Stub.Object'Class;
      Traveller_Utils :
         access Active.Traveller.Utils.Object'Class;
   end record;

   Instance : District.Reference;
   Id : constant String := (EV.Value (Name => "CITY_NODE_ID"));

end Reactive.District;
