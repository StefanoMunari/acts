with Ada.Containers.Ordered_Maps;

with GNATCOLL.JSON;

with Reactive.Directory.Bikeway_Directory;
with Reactive.Directory.Footway_Directory;
with Reactive.Directory.Intersection_Directory;
with Reactive.Directory.Lane_Directory;
with Reactive.Directory.Roadway_Directory;
with Reactive.Directory.Street_Directory;
with Reactive.Directory.Stretch_Directory;
with Reactive.Infrastructure;
with Reactive.Infrastructure.Intersection;
with Reactive.Infrastructure.Lane;
with Reactive.Infrastructure.Street;
with Reactive.Infrastructure.Street_Related_Infrastructure;
with Reactive.Infrastructure.Stretch;
with Reactive.Infrastructure.Way;
with Reactive.Infrastructure.Way.Bikeway;
with Reactive.Infrastructure.Way.Footway;
with Reactive.Infrastructure.Way.Roadway;
with Reactive.Treadable;

with Shared.Shared_References_Street;

package Reactive.Infrastructure_Registry is

-- libs
   package G_JSON       renames GNATCOLL.JSON;
-- reactive
   package Stretch      renames Reactive.Infrastructure.Stretch;
   package Street_Related_Infrastructure
      renames Reactive.Infrastructure.Street_Related_Infrastructure;
   package Street       renames Reactive.Infrastructure.Street;
   package Way          renames Reactive.Infrastructure.Way;
   package Roadway      renames Reactive.Infrastructure.Way.Roadway;
   package Bikeway      renames Reactive.Infrastructure.Way.Bikeway;
   package Footway      renames Reactive.Infrastructure.Way.Footway;
   package Intersection renames Reactive.Infrastructure.Intersection;
   package Lane         renames Reactive.Infrastructure.Lane;
-- shared
   package SR_Street_Pkg renames Shared.Shared_References_Street;

   use Reactive.Infra_Id_Type;

   type Object (<>) is tagged limited private;
   type Reference is access all Infrastructure_Registry.Object'Class;

   function Get_Instance return Infrastructure_Registry.Reference;

   function Contains_Infrastructure (
      This              : in Infrastructure_Registry.Object;
      Infrastructure_Id : in Infra_Id)
   return Boolean;

   function Contains_Treadable (
      This         : in Infrastructure_Registry.Object;
      Treadable_Id : in Infra_Id)
   return Boolean;

   function Find_Infrastructure_By_Id (
      This              : in Infrastructure_Registry.Object;
      Infrastructure_Id : in Infra_Id)
   return Infrastructure.Reference;

   function Find_Treadable_By_Id (
      This         : in Infrastructure_Registry.Object;
      Treadable_Id : in Infra_Id)
   return Treadable.Reference;

   function Find_Intersection_By_Id (
      This            : in Infrastructure_Registry.Object;
      Intersection_Id : in Infra_Id)
   return Intersection.Reference;

   function Find_Street_By_Id (
      This      : in Infrastructure_Registry.Object;
      Street_Id : in Infra_Id)
   return Street.Reference;

   function Find_Way_By_Id (
      This   : in Infrastructure_Registry.Object;
      Way_Id : in Infra_Id)
   return Way.Reference;

   function Find_Roadway_By_Id (
      This       : in Infrastructure_Registry.Object;
      Roadway_Id : in Infra_Id)
   return Roadway.Reference;

   function Find_Footway_By_Id (
      This       : in Infrastructure_Registry.Object;
      Footway_Id : in Infra_Id)
   return Footway.Reference;

   function Find_Bikeway_By_Id (
      This       : in Infrastructure_Registry.Object;
      Bikeway_Id : in Infra_Id)
   return Bikeway.Reference;

   function Find_Lane_By_Id (
      This    : in Infrastructure_Registry.Object;
      Lane_Id : in Infra_Id)
   return Lane.Reference;

   function Find_Stretch_By_Id (
      This       : in Infrastructure_Registry.Object;
      Stretch_Id : in Infra_Id)
   return Stretch.Reference;

   procedure Add_Intersection (
      This           : in out Infrastructure_Registry.Object;
      Infrastructure :
         aliased in out Reactive.Infrastructure.Intersection.Object'Class;
      Added          : out Boolean);

   procedure Add_Street (
      This      : in out Infrastructure_Registry.Object;
      SR_Street : in out SR_Street_Pkg.Shared_Reference;
      Added     :    out Boolean);

   procedure Add_Roadway (
      This           : in out Infrastructure_Registry.Object;
      Infrastructure :
         aliased in out Reactive.Infrastructure.Way.Roadway.Reference;
      Added          : out Boolean);

   procedure Add_Footway (
      This           : in out Infrastructure_Registry.Object;
      Infrastructure :
         aliased in out Reactive.Infrastructure.Way.Footway.Reference;
      Added          : out Boolean);

   procedure Add_Bikeway (
      This           : in out Infrastructure_Registry.Object;
      Infrastructure :
         aliased in out Reactive.Infrastructure.Way.Bikeway.Reference;
      Added          : out Boolean);

   procedure Add_Lane (
      This           : in out Infrastructure_Registry.Object;
      Infrastructure :
         aliased in out Reactive.Infrastructure.Lane.Reference;
      Added          : out Boolean);

   procedure Add_Stretch (
      This           : in out Infrastructure_Registry.Object;
      Infrastructure :
         aliased in out Reactive.Infrastructure.Stretch.Reference;
      Added          : out Boolean);

   procedure Clear (This : in out Infrastructure_Registry.Object);

   not overriding
   function Dump (This : in Infrastructure_Registry.Object)
   return G_JSON.JSON_Value;

private
   type Object is tagged limited record
      Intersection_Directory :
         access Reactive.Directory.Intersection_Directory.Directory
            := new Reactive.Directory.Intersection_Directory.Directory;
      Street_Directory :
         access Reactive.Directory.Street_Directory.Directory
            := new Reactive.Directory.Street_Directory.Directory;
      Roadway_Directory :
         access Reactive.Directory.Roadway_Directory.Directory
            := new Reactive.Directory.Roadway_Directory.Directory;
      Footway_Directory :
         access Reactive.Directory.Footway_Directory.Directory
            := new Reactive.Directory.Footway_Directory.Directory;
      Bikeway_Directory :
         access Reactive.Directory.Bikeway_Directory.Directory
            := new Reactive.Directory.Bikeway_Directory.Directory;
      Lane_Directory :
         access Reactive.Directory.Lane_Directory.Directory
            := new Reactive.Directory.Lane_Directory.Directory;
      Stretch_Directory :
         access Reactive.Directory.Stretch_Directory.Directory
            := new Reactive.Directory.Stretch_Directory.Directory;
   end record;

   Instance : Infrastructure_Registry.Reference;

end Reactive.Infrastructure_Registry;
