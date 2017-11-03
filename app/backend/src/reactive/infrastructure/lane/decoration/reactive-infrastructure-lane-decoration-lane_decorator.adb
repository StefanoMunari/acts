package body Reactive.Infrastructure.Lane.Decoration.Lane_Decorator is

   procedure Init (This     : in out Lane_Decorator.Object;
                   Lane_Ref : in     Reactive.Infrastructure.Lane.Reference) is
   begin -- Init
      This.Lane_Ref := Lane_Ref;
   end Init;

   not overriding
   function Get_Lane_Ref (This : in Lane_Decorator.Object)
   return Reactive.Infrastructure.Lane.Reference is (This.Lane_Ref);

   function Get_Id (This : in Lane_Decorator.Object) return Infra_Id
   is (This.Lane_Ref.Get_Id);

   procedure Enter (
      This         : in out Lane_Decorator.Object;
      Traveller_Id : in Agent.Agent_Id) is
   begin
      This.Lane_Ref.Enter (Traveller_Id);
   end Enter;

   function "=" (This, Other : in Lane_Decorator.Object) return Boolean
   is (This.Get_Id = Other.Get_Id);

   function Find_Street (This : in Lane_Decorator.Object)
                         return Infra_Id
   is (This.Lane_Ref.Find_Street);

   function Find_Intersections (This : in Lane_Decorator.Object)
                                return Infra_Id_Set.Set
   is (This.Lane_Ref.Find_Intersections);

   procedure Append_Stretch (
      This       : in out Lane_Decorator.Object;
      Stretch_Id : in     Infra_Id;
      Added      :    out Boolean) is
   begin
      This.Lane_Ref.Append_Stretch (Stretch_Id, Added);
   end Append_Stretch;

   function Count_Stretches (This : in Lane_Decorator.Object) return Natural
   is (This.Lane_Ref.Count_Stretches);

   procedure Find_Stretch_Position (
      This             : in     Lane_Decorator.Object;
      Stretch_Id       : in     Infra_Id;
      Stretch_Position :    out Natural;
      Found            :    out Boolean) is
   begin
      This.Lane_Ref.Find_Stretch_Position (Stretch_Id, Stretch_Position, Found);
   end Find_Stretch_Position;

   function Get_Stretch_By_Position (
      This             : in Lane_Decorator.Object;
      Stretch_Position : in Natural)
   return Infra_Id
   is (This.Lane_Ref.Get_Stretch_By_Position (Stretch_Position));

   function Get_Direction (This : Lane_Decorator.Object)
                           return Direction.Straight
   is (This.Lane_Ref.Get_Direction);

   function Is_Contained_By (This         : in Lane_Decorator.Object;
                             Container_Id : in Infra_Id)
   return Boolean
   is (This.Lane_Ref.Is_Contained_By (Container_Id));

   procedure Set_Way (This : in out Lane_Decorator.Object; Way_Id : in Infra_Id) is
   begin
      This.Lane_Ref.Set_Way (Way_Id);
   end Set_Way;

   procedure Add_Intersection (
      This            : in out Lane_Decorator.Object;
      Intersection_Id : in     Infra_Id;
      Added           :    out Boolean) is
   begin
      This.Lane_Ref.Add_Intersection (Intersection_Id, Added);
   end Add_Intersection;

end Reactive.Infrastructure.Lane.Decoration.Lane_Decorator;
