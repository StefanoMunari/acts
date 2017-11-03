with Ada.Containers.Ordered_Maps;

package Reactive.District.Mock is

   type Object (<>) is new District.Object with private;
   type Reference is access all District.Mock.Object'Class;

   package Tredable_By_Id is
     new Ada.Containers.Ordered_Maps
       (Key_Type     => Infra_Id,
        Element_Type => Infrastructure.Reference,
        "<"          => "<",
        "="          => Infrastructure."=");

   package Traveller_By_Id is
     new Ada.Containers.Ordered_Maps
       (Key_Type     => Agent.Agent_Id,
        Element_Type => Traveller.Reference,
        "<"          => Agent."<",
        "="          => Traveller."=");

   package Host_By_Id is
     new Ada.Containers.Ordered_Maps
       (Key_Type     => Infra_Id,
        Element_Type => Host_Pkg.Reference,
        "<"          => "<",
        "="          => Host_Pkg."=");

   function Create return District.Mock.Reference;

   overriding
   function Contains_Infrastructure (
      This              : in District.Mock.Object;
      Infrastructure_Id : in Infra_Id)
   return Boolean;

   overriding
   function Find_Infrastructure_By_Id (
      This              : in District.Mock.Object;
      Infrastructure_Id : in Infra_Id)
   return Infrastructure.Reference;

   overriding
   function Find_Intersection_By_Id (This            : in District.Mock.Object;
                                     Intersection_Id : in Infra_Id)
   return Intersection.Reference;

   overriding
   function Find_Street_By_Id (
      This                             : in District.Mock.Object;
      Street_Related_Infrastructure_Id : in Infra_Id)
   return Street.Reference;

   overriding
   function Find_Way_By_Id (This   : in District.Mock.Object;
                            Way_Id : in Infra_Id)
   return Way.Reference;

   overriding
   function Find_Roadway_By_Id (This       : in District.Mock.Object;
                                Roadway_Id : in Infra_Id)
   return Roadway.Reference;

   overriding
   function Find_Footway_By_Id (This       : in District.Mock.Object;
                                Footway_Id : in Infra_Id)
   return Footway.Reference;

   overriding
   function Find_Bikeway_By_Id (This       : in District.Mock.Object;
                                Bikeway_Id : in Infra_Id)
   return Bikeway.Reference;

   overriding
   function Find_Lane_By_Id (This    : in District.Mock.Object;
                             Lane_Id : in Infra_Id)
   return Lane.Reference;

   overriding
   function Find_Stretch_By_Id (This       : in District.Mock.Object;
                                Stretch_Id : in Infra_Id)
   return Stretch.Reference;

   overriding
   function Find_Traveller_By_Id (This         : in District.Mock.Object;
                                  Traveller_Id : in Agent.Agent_Id)
   return Traveller.Reference;

   overriding
   function Find_Host_By_Id (This    : in District.Mock.Object;
                             Host_Id : in Infra_Id)
   return Host_Pkg.Reference;

   overriding
   procedure Add_Intersection (
      This : in out District.Mock.Object;
      Infrastructure : aliased in out Reactive.Infrastructure.Intersection.Object'Class;
      Added          : out Boolean);

   overriding
   procedure Add_Street (
      This      : in out District.Mock.Object;
      SR_Street : in out SR_Street_Pkg.Shared_Reference;
      Added     :    out Boolean);

   overriding
   procedure Add_Roadway (
      This : in out District.Mock.Object;
      Infrastructure :
         aliased in out Reactive.Infrastructure.Way.Roadway.Reference;
      Added          : out Boolean);

   overriding
   procedure Add_Footway (
      This : in out District.Mock.Object;
      Infrastructure :
         aliased in out Reactive.Infrastructure.Way.Footway.Reference;
      Added          : out Boolean);

   overriding
   procedure Add_Bikeway (
      This           : in out District.Mock.Object;
      Infrastructure :
         aliased in out Reactive.Infrastructure.Way.Bikeway.Reference;
      Added          : out Boolean);

   overriding
   procedure Add_Lane (
      This           : in out District.Mock.Object;
      Infrastructure :
         aliased in out Reactive.Infrastructure.Lane.Reference;
      Added          : out Boolean);

   overriding
   procedure Add_Stretch (
      This           : in out District.Mock.Object;
      Infrastructure :
         aliased in out Reactive.Infrastructure.Stretch.Reference;
      Added          :    out Boolean);

   overriding
   procedure Add_Traveller (
      This      : in out District.Mock.Object;
      Traveller : in out Active.Traveller.Reference;
      Added     :    out Boolean);

   overriding
   procedure Add_Host (
      This     :         in out District.Mock.Object;
      Host_Ref : aliased in out Host_Pkg.Object'Class;
      Added    :            out Boolean);

   overriding
   procedure Remove_Traveller (
      This         : in out District.Mock.Object;
      Traveller_Id : in Agent.Agent_Id;
      Removed      : out Boolean);

private
   type Mock_Values_Collection is record
      Roadmap    : Tredable_By_Id.Map;
      Population : Traveller_By_Id.Map;
      Buildings  : Host_By_Id.Map;
   end record;

   type Object is new District.Object with record
      Mock_Values : Mock_Values_Collection;
   end record;

end Reactive.District.Mock;
