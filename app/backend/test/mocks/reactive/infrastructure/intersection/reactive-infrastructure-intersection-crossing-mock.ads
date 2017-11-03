with Active.Agent;

package Reactive.Infrastructure.Intersection.Crossing.Mock is

   package Agent renames Active.Agent;

   type Object (<>) is
     new Intersection.Crossing.Object
   with private;
   type Reference is access all Intersection.Crossing.Mock.Object'Class;

   function Create return Intersection.Crossing.Mock.Reference;

   overriding
   procedure Cross
     (This         : in out Intersection.Crossing.Mock.Object;
      Traveller_Id : in Agent.Agent_Id;
      Direction    : in Shared.Direction.Any;
      Crossed : out Boolean) is null;

   overriding
   procedure Initialize (This : in out Intersection.Crossing.Mock.Object)
   is null;

private
   type Object is
     new Intersection.Crossing.Object
   with null record;

end Reactive.Infrastructure.Intersection.Crossing.Mock;
