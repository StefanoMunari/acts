with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Ordered_Maps;

with Active.Agent;
with Active.Traveller.Utils;
limited with Active.Traveller.Pedestrian;
limited with Active.Traveller.Vehicle;
limited with Active.Traveller.Vehicle.Bicycle;

with Reactive.Infrastructure.Street_Related_Infrastructure;
with Reactive.Infrastructure.Lane;
limited with Reactive.Infrastructure.Way.Roadway.Utils;
limited with Reactive.Infrastructure.Way.Footway.Utils;
limited with Reactive.Infrastructure.Way.Bikeway.Utils;

with Shared.Containable;
with Shared.Direction;
with Shared.Infra_Id_List;
with Shared.Infra_Id_Set;

package Reactive.Infrastructure.Street is

   package Agent         renames Active.Agent;
   package Containable   renames Shared.Containable;
   package Direction     renames Shared.Direction;
   package Infra_Id_List renames Shared.Infra_Id_List;
   package Infra_Id_Set  renames Shared.Infra_Id_Set;
   use Reactive.Infra_Id_Type;

   type Object (<>) is
     new Street_Related_Infrastructure.Object
     and Infrastructure.Object
     and Containable.Object
   with private;
   type Reference is access all Street.Object'Class;

   function Create (
      Id              : in     Infra_Id;
      Orientation     : in     Direction.Orientation;
      Roadway_Utils   : access Roadway.Utils.Object'Class := null;
      Footway_Utils   : access Footway.Utils.Object'Class := null;
      Bikeway_Utils   : access Bikeway.Utils.Object'Class := null;
      Traveller_Utils : access Active.Traveller.Utils.Object'Class := null)
   return Street.Reference;

   not overriding
   function Is_Treadable_In_Direction (This      : in Street.Object;
                                       Direction : Shared.Direction.Cardinal)
   return Boolean;

   not overriding
   function Is_Not_Treadable_In_Direction (
      This      : in Street.Object;
      Direction : Shared.Direction.Cardinal)
   return Boolean;

   overriding
   function Get_Id (This : in Street.Object) return Infra_Id;

   not overriding
   function Get_Orientation (This: in Street.Object)
   return Direction.Orientation;

   not overriding
   procedure Add_Roadway (This       :    out Street.Object;
                          Roadway_Id : in     Infra_Id);

   not overriding
   procedure Add_Footway (This       :    out Street.Object;
                          Footway_Id : in     Infra_Id);

   not overriding
   procedure Add_Bikeway (This       :    out Street.Object;
                          Bikeway_Id : in     Infra_Id);

   not overriding
   function Get_Footways (This : Street.Object)
   return Infra_Id_List.List;

   overriding
   function "=" (This, Outher : in Street.Object) return Boolean;

   overriding
   function Find_Street (This : in Street.Object)
   return Infra_Id;

   not overriding
   function Find_Lanes_By_Direction (This             : in Street.Object;
                                     Travel_Direction : in Direction.Straight)
   return Infra_Id_Set.Set;

   overriding
   function Is_Contained_By (This         : in Street.Object;
                             Container_Id : in Infra_Id)
   return Boolean;

   overriding
   function Dump (This : Street.Object) return G_JSON.JSON_Value;

-- JSON FIELDS CONSTANTS
   function Id_Field          return String is ("id");
   function Orientation_Field return String is ("orientation");
   function Bikeways_Field    return String is ("bikeways");
   function Footways_Field    return String is ("footways");
   function Roadways_Field    return String is ("roadways");

private

   type Object is
     new Street_Related_Infrastructure.Object
     and Infrastructure.Object
     and Containable.Object with record
      Id            : Infra_Id;
      Orientation   : Direction.Orientation;
      Roadway_Ids   : Infra_Id_List.List;
      Footway_Ids   : Infra_Id_List.List;
      Bikeway_Ids   : Infra_Id_List.List;
      Roadway_Utils   : access Roadway.Utils.Object'Class;
      Footway_Utils   : access Footway.Utils.Object'Class;
      Bikeway_Utils   : access Bikeway.Utils.Object'Class;
      Traveller_Utils : access Active.Traveller.Utils.Object'Class;
   end record;

end Reactive.Infrastructure.Street;
