with Mock.Exceptions;

use Mock.Exceptions;

package body Reactive.Infrastructure.Lane.Footway_Lane.Mock is

   function Create return Footway_Lane.Mock.Reference
   is (new Footway_Lane.Mock.Object);

   procedure Append_Stretch
     (This       : in out Footway_Lane.Mock.Object;
      Stretch_Id : in     Infra_Id;
      Added      :    out Boolean) is
   begin
      Added := This.Return_Values.Append_Stretch;
   end Append_Stretch;

   function Count_Stretches (This : in Footway_Lane.Mock.Object) return Natural
   is (This.Return_Values.Count_Stretches);

   procedure Find_Stretch_Position
     (This             : in     Footway_Lane.Mock.Object;
      Stretch_Id       : in     Infra_Id;
      Stretch_Position :    out Natural;
      Found    : out Boolean) is
   begin
      Stretch_Position := This.Return_Values.Find_Stretch_Position;
      Found := This.Return_Values.Find_Stretch_Position_Found;
   end Find_Stretch_Position;

   function "=" (This, Outher : in Footway_Lane.Mock.Object) return Boolean
   is (This.Return_Values.Equality_Operator);

   function Find_Street (This : in Footway_Lane.Mock.Object)
      return Infra_Id
   is (This.Return_Values.Find_Street);

   function Get_Id (This : in Footway_Lane.Mock.Object) return Infra_Id
   is (This.Return_Values.Get_Id);

   function Get_Direction (This : Footway_Lane.Mock.Object)
      return Shared.Direction.Straight
   is (This.Return_Values.Get_Direction);

   function Find_Intersections (
      This : in Footway_Lane.Mock.Object) return Infra_Id_Set.Set
   is (This.Return_Values.Find_Intersections);

   procedure Add_Intersection
     (This            : in out Footway_Lane.Mock.Object;
      Intersection_Id : in     Infra_Id;
      Added           :    out Boolean) is
   begin
      Added := This.Return_Values.Add_Intersection;
   end Add_Intersection;

   function Is_Contained_By (This         : in Footway_Lane.Mock.Object;
                             Container_Id : in Infra_Id) return Boolean
   is (This.Return_Values.Is_Contained_By);

   procedure Set_Return_Value_For_Equality_Operator
     (This         : in out Footway_Lane.Mock.Object;
      Return_Value : in     Boolean) is
   begin
      This.Return_Values.Equality_Operator := Return_Value;
   end Set_Return_Value_For_Equality_Operator;

   procedure Set_Return_Value_For_Find_Street
     (This         : in out Footway_Lane.Mock.Object;
      Return_Value : in     Infra_Id) is
   begin
      This.Return_Values.Find_Street := Return_Value;
   end Set_Return_Value_For_Find_Street;

   procedure Set_Return_Value_For_Get_Id
     (This         : in out Footway_Lane.Mock.Object;
      Return_Value : in     Infra_Id) is
   begin
      This.Return_Values.Get_Id := Return_Value;
   end Set_Return_Value_For_Get_Id;

   procedure Set_Return_Value_For_Find_Intersections
     (This         : in out Footway_Lane.Mock.Object;
      Return_Value : in     Infra_Id_Set.Set) is
   begin
      This.Return_Values.Find_Intersections := Return_Value;
   end Set_Return_Value_For_Find_Intersections;

   procedure Set_Added_Value_For_Append_Stretch
     (This  : in out Footway_Lane.Mock.Object;
      Added : in     Boolean) is
   begin
      This.Return_Values.Append_Stretch := Added;
   end Set_Added_Value_For_Append_Stretch;

   procedure Set_Return_Value_For_Count_Stretches
     (This         : in out Footway_Lane.Mock.Object;
      Return_Value : in     Natural) is
   begin
      This.Return_Values.Count_Stretches := Return_Value;
   end Set_Return_Value_For_Count_Stretches;

   procedure Set_Position_Value_For_Find_Stretch_Position
     (This     : in out Footway_Lane.Mock.Object;
      Position : in     Natural) is
   begin
      This.Return_Values.Find_Stretch_Position := Position;
   end Set_Position_Value_For_Find_Stretch_Position;

   procedure Set_Found_Value_For_Find_Stretch_Position
     (This  : in out Footway_Lane.Mock.Object;
      Found : in     Boolean) is
   begin
      This.Return_Values.Find_Stretch_Position_Found := Found;
   end Set_Found_Value_For_Find_Stretch_Position;

   procedure Set_Return_Value_For_Get_Direction
     (This         : in out Footway_Lane.Mock.Object;
      Return_Value : in     Direction.Straight) is
   begin
      This.Return_Values.Get_Direction := Return_Value;
   end Set_Return_Value_For_Get_Direction;

   procedure Set_Return_Value_For_Is_Contained_By
     (This         : in out Footway_Lane.Mock.Object;
      Return_Value : in     Boolean) is
   begin
      This.Return_Values.Is_Contained_By := Return_Value;
   end Set_Return_Value_For_Is_Contained_By;

   procedure Set_Added_Value_For_Add_Intersection
     (This  : in out Footway_Lane.Mock.Object;
      Added : in     Boolean) is
   begin
      This.Return_Values.Add_Intersection := Added;
   end Set_Added_Value_For_Add_Intersection;

end Reactive.Infrastructure.Lane.Footway_Lane.Mock;
