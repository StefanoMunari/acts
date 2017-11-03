-- core
with Ada.Finalization;
-- gnatcoll libs
with GNATCOLL.JSON;

with Reactive.District;
with Reactive.Infrastructure.Factory.Street_Factory;
with Reactive.Infrastructure.Way.Bikeway;
with Reactive.Infrastructure.Way.Bikeway.Utils;
with Reactive.Infrastructure.Way.Footway;
with Reactive.Infrastructure.Way.Footway.Utils;
with Reactive.Infrastructure.Way.Roadway;
with Reactive.Infrastructure.Way.Roadway.Utils;

with Shared.Infra_Id_List;

package Reactive.Infrastructure.Build.Street_Builder is

   package Street_Factory
      renames Reactive.Infrastructure.Factory.Street_Factory;
   package Infra_Id_List renames Shared.Infra_Id_List;
   package G_JSON        renames GNATCOLL.JSON;
   package Way_Pkg       renames Reactive.Infrastructure.Way;
   package Bikeway_Pkg   renames Way_Pkg.Bikeway;
   package Footway_Pkg   renames Way_Pkg.Footway;
   package Roadway_Pkg   renames Way_Pkg.Roadway;
   use Reactive.Infra_Id_Type;

   type Object is new Ada.Finalization.Controlled with private;
   type Reference is access all Object'Class;

   function Create (
      District        : access Reactive.District.Object'Class := null;
      Bikeway_Factory : access Street_Factory.Object'Class    := null;
      Footway_Factory : access Street_Factory.Object'Class    := null;
      Roadway_Factory : access Street_Factory.Object'Class    := null;
      Roadway_Utils   : access Roadway_Pkg.Utils.Object'Class := null;
      Footway_Utils   : access Footway_Pkg.Utils.Object'Class := null;
      Bikeway_Utils   : access Bikeway_Pkg.Utils.Object'Class := null)
   return Street_Builder.Reference;

   not overriding
   procedure Reset (This : in out Street_Builder.Object);

   not overriding
   function With_Bikeway (This : in out Street_Builder.Object;
                          JSON : in     G_JSON.JSON_Value)
   return Infra_Id;

   not overriding
   function With_Footway (This : in out Street_Builder.Object;
                          JSON : in     G_JSON.JSON_Value)
   return Infra_Id;

   not overriding
   function With_Roadway (This : in out Street_Builder.Object;
                          JSON : in     G_JSON.JSON_Value)
   return Infra_Id;

   not overriding
   function Get_Street (This : in out Street_Builder.Object;
                        JSON : in     G_JSON.JSON_Value)
   return Infra_Id;

private
   type Object is new Ada.Finalization.Controlled with record
      Bikeway_Factory  : access Street_Factory.Object'Class := null;
      Footway_Factory  : access Street_Factory.Object'Class := null;
      Roadway_Factory  : access Street_Factory.Object'Class := null;
      Bikeways         : Infra_Id_List.List;
      Footways         : Infra_Id_List.List;
      Roadways         : Infra_Id_List.List;
      Bikeway_Utils    : access Bikeway_Pkg.Utils.Object'Class := null;
      Footway_Utils    : access Footway_Pkg.Utils.Object'Class := null;
      Roadway_Utils    : access Roadway_Pkg.Utils.Object'Class := null;
      District         : access Reactive.District.Object'Class;
   end record;

   function Get_Stretches (This      : in out Street_Builder.Object;
                           Stretches : in     G_JSON.JSON_Array;
                           Factory   :        Street_Factory.Reference;
                           Lane_Id   : in     Infra_Id)
   return Infra_Id_List.List;

   function Get_Lanes (This  : in out Street_Builder.Object;
                       Lanes : in     G_JSON.JSON_Array;
                       Factory  : Street_Factory.Reference)
   return Infra_Id_List.List;

end Reactive.Infrastructure.Build.Street_Builder;
