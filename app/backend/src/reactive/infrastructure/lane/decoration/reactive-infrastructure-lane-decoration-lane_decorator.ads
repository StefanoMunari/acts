with Reactive.Infrastructure.Lane;

package Reactive.Infrastructure.Lane.Decoration.Lane_Decorator is

   type Object is
     abstract new Reactive.Infrastructure.Lane.Object
   with private;
   type Reference is access all Object'Class;

   not overriding
   procedure Init (This     : in out Lane_Decorator.Object;
                   Lane_Ref : in     Reactive.Infrastructure.Lane.Reference);

   not overriding
   function Get_Lane_Ref (This : in Lane_Decorator.Object)
   return Reactive.Infrastructure.Lane.Reference;

   overriding
   function Get_Id (This : in Lane_Decorator.Object) return Infra_Id;

   overriding
   procedure Enter (
      This         : in out Lane_Decorator.Object;
      Traveller_Id : in Agent.Agent_Id);

   overriding
   function "=" (This, Other : in Lane_Decorator.Object) return Boolean;

   overriding
   function Find_Street (This : in Lane_Decorator.Object)
                         return Infra_Id;

   overriding
   function Find_Intersections (This : in Lane_Decorator.Object)
                                return Infra_Id_Set.Set;

   overriding
   procedure Append_Stretch (
      This       : in out Lane_Decorator.Object;
      Stretch_Id : in     Infra_Id;
      Added      :    out Boolean);

   overriding
   function Count_Stretches (This : in Lane_Decorator.Object) return Natural;

   overriding
   procedure Find_Stretch_Position (
      This             : in     Lane_Decorator.Object;
      Stretch_Id       : in     Infra_Id;
      Stretch_Position :    out Natural;
      Found            :    out Boolean);

   overriding
   function Get_Stretch_By_Position (
      This             : in Lane_Decorator.Object;
      Stretch_Position : in Natural)
   return Infra_Id;

   overriding
   function Get_Direction (This : Lane_Decorator.Object)
                           return Direction.Straight;

   overriding
   function Is_Contained_By (This         : in Lane_Decorator.Object;
                             Container_Id : in Infra_Id)
   return Boolean;

   overriding
   procedure Set_Way (This : in out Lane_Decorator.Object; Way_Id : in Infra_Id);

   overriding
   procedure Add_Intersection (
      This            : in out Lane_Decorator.Object;
      Intersection_Id : in     Infra_Id;
      Added           :    out Boolean);

private

   type Object is
     abstract new Reactive.Infrastructure.Lane.Object with record
      Lane_Ref : Reactive.Infrastructure.Lane.Reference;
   end record;

end Reactive.Infrastructure.Lane.Decoration.Lane_Decorator;
