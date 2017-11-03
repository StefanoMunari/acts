with Ada.Containers.Ordered_Maps;

with Shared.Infra_Id_Set;

package Reactive.Infrastructure_Registry.Mock is

   package Treadable renames Reactive.Treadable;
   package Infra_Id_Set renames Shared.Infra_Id_Set;

   type Object (<>) is new Infrastructure_Registry.Object with private;
   type Reference is access all Infrastructure_Registry.Mock.Object'Class;

   package Infra_By_Id is
     new Ada.Containers.Ordered_Maps
       (Key_Type     => Infra_Id,
        Element_Type => Infrastructure.Reference,
        "<"          => "<",
        "="          => Infrastructure."=");

   package Treadable_By_Id is
     new Ada.Containers.Ordered_Maps
       (Key_Type     => Infra_Id,
        Element_Type => Treadable.Reference,
        "<"          => "<",
        "="          => Treadable."=");

   function Create return Infrastructure_Registry.Mock.Reference;

   overriding
   function Contains_Infrastructure (
      This              : in Mock.Object;
      Infrastructure_Id : in Infra_Id)
   return Boolean;

   overriding
   function Contains_Treadable (
      This         : in Mock.Object;
      Treadable_Id : in Infra_Id)
   return Boolean;

   overriding
   function Find_Infrastructure_By_Id (
      This              : in Mock.Object;
      Infrastructure_Id : in Infra_Id)
   return Infrastructure.Reference;

   overriding
   function Find_Treadable_By_Id (
      This         : in Infrastructure_Registry.Mock.Object;
      Treadable_Id : in Infra_Id)
   return Treadable.Reference;

   overriding
   function Find_Intersection_By_Id (
      This            : in Mock.Object;
      Intersection_Id : in Infra_Id)
   return Intersection.Reference;

   overriding
   function Find_Street_By_Id (
      This      : in Mock.Object;
      Street_Id : in Infra_Id)
   return Street.Reference;

   overriding
   function Find_Way_By_Id (
      This   : in Mock.Object;
      Way_Id : in Infra_Id)
   return Way.Reference;

   overriding
   function Find_Roadway_By_Id (
      This       : in Mock.Object;
      Roadway_Id : in Infra_Id)
   return Roadway.Reference;

   overriding
   function Find_Footway_By_Id (
      This       : in Mock.Object;
      Footway_Id : in Infra_Id)
   return Footway.Reference;

   overriding
   function Find_Bikeway_By_Id (
      This       : in Mock.Object;
      Bikeway_Id : in Infra_Id)
   return Bikeway.Reference;

   overriding
   function Find_Lane_By_Id (
      This    : in Mock.Object;
      Lane_Id : in Infra_Id)
   return Lane.Reference;

   overriding
   function Find_Stretch_By_Id (
      This : in Mock.Object;
      Stretch_Id : in Infra_Id)
   return Stretch.Reference;

   overriding
   procedure Add_Intersection (
      This           : in out Mock.Object;
      Infrastructure :
         aliased in out Reactive.Infrastructure.Intersection.Object'Class;
      Added          :    out Boolean);

   overriding
   procedure Add_Street (
      This      : in out Mock.Object;
      SR_Street : in out SR_Street_Pkg.Shared_Reference;
      Added     :    out Boolean);

   overriding
   procedure Add_Roadway (
      This : in out Infrastructure_Registry.Mock.Object;
      Infrastructure :
         aliased in out Reactive.Infrastructure.Way.Roadway.Reference;
      Added          : out Boolean);

   overriding
   procedure Add_Footway (
      This           : in out Infrastructure_Registry.Mock.Object;
      Infrastructure :
         aliased in out Reactive.Infrastructure.Way.Footway.Reference;
      Added          : out Boolean);

   overriding
   procedure Add_Bikeway (
      This           : in out Infrastructure_Registry.Mock.Object;
      Infrastructure :
         aliased in out Reactive.Infrastructure.Way.Bikeway.Reference;
      Added          :    out Boolean);

   overriding
   procedure Add_Lane (
      This           : in out Infrastructure_Registry.Mock.Object;
      Infrastructure :
         aliased in out Reactive.Infrastructure.Lane.Reference;
      Added          :    out Boolean);

   overriding
   procedure Add_Stretch (
      This           : in out Infrastructure_Registry.Mock.Object;
      Infrastructure :
         aliased in out Reactive.Infrastructure.Stretch.Reference;
      Added          :    out Boolean);

   not overriding
   procedure Set_Return_Value_For_Contains (
      This         : in out Infrastructure_Registry.Mock.Object;
      Return_Value : in     Infra_Id);

   not overriding
   procedure Set_Return_Value_For_Find (
      This  : in out Infrastructure_Registry.Mock.Object;
      Key   : in     Infra_Id;
      Value : in     Infrastructure.Reference);

   not overriding
   procedure Set_Return_Value_For_Find_Treadable (
      This  : in out Infrastructure_Registry.Mock.Object;
      Key   : in     Infra_Id;
      Value : in     Treadable.Reference);

private
   type Return_Values_Collection is record
      Find           : Infra_By_Id.Map;
      Find_Treadable : Treadable_By_Id.Map;
   end record;

   type Mock_Values_Collection is record
      Roadmap : Infra_Id_Set.Set;
   end record;

   type Object is new Infrastructure_Registry.Object with record
      Mock_Values   : Mock_Values_Collection;
      Return_Values : Return_Values_Collection;
   end record;

end Reactive.Infrastructure_Registry.Mock;
