with Active.Agent;

package Reactive.Infrastructure.Lane.Footway_Lane.Mock is

   package Agent renames Active.Agent;

   type Object is new Lane.Footway_Lane.Object with private;
   type Reference is access all Footway_Lane.Mock.Object'Class;

   function Create return Footway_Lane.Mock.Reference;

   overriding
   function "=" (This, Outher : in Footway_Lane.Mock.Object) return Boolean;

   overriding
   function Find_Street (This : in Footway_Lane.Mock.Object)
      return Infra_Id;

   overriding
   function Get_Id (This : in Footway_Lane.Mock.Object) return Infra_Id;

   overriding
   function Find_Intersections (This : in Footway_Lane.Mock.Object)
      return Infra_Id_Set.Set;

   overriding
   procedure Append_Stretch (
      This       : in out Footway_Lane.Mock.Object;
      Stretch_Id : in Infra_Id;
      Added      : out Boolean);

   overriding
   function Count_Stretches (This : in Footway_Lane.Mock.Object)
      return Natural;

   overriding
   procedure Find_Stretch_Position (
      This             : in     Footway_Lane.Mock.Object;
      Stretch_Id       : in     Infra_Id;
      Stretch_Position :    out Natural;
      Found            :    out Boolean);

   overriding
   function Get_Direction (This : Footway_Lane.Mock.Object)
      return Direction.Straight;

   overriding
   function Is_Contained_By (This         : in Footway_Lane.Mock.Object;
                             Container_Id : in Infra_Id) return Boolean;

   overriding
   procedure Set_Way (This   : in out Footway_Lane.Mock.Object;
                      Way_Id : in     Infra_Id)
   is null;

   overriding
   procedure Add_Intersection (
      This            : in out Footway_Lane.Mock.Object;
      Intersection_Id : in     Infra_Id;
      Added           :    out Boolean);

   procedure Set_Return_Value_For_Equality_Operator (
      This         : in out Footway_Lane.Mock.Object;
      Return_Value :    in Boolean);

   procedure Set_Return_Value_For_Find_Street (
      This         : in out Footway_Lane.Mock.Object;
      Return_Value : in     Infra_Id);

   procedure Set_Return_Value_For_Get_Id (
      This         : in out Footway_Lane.Mock.Object;
      Return_Value : in     Infra_Id);

   procedure Set_Return_Value_For_Find_Intersections (
      This         : in out Footway_Lane.Mock.Object;
      Return_Value : in     Infra_Id_Set.Set);

   procedure Set_Added_Value_For_Append_Stretch (
      This  : in out Footway_Lane.Mock.Object;
      Added : in     Boolean);

   procedure Set_Return_Value_For_Count_Stretches (
      This         : in out Footway_Lane.Mock.Object;
      Return_Value : in     Natural);

   procedure Set_Position_Value_For_Find_Stretch_Position (
      This     : in out Footway_Lane.Mock.Object;
      Position : in     Natural);

   procedure Set_Found_Value_For_Find_Stretch_Position (
      This  : in out Footway_Lane.Mock.Object;
      Found : in     Boolean);

   procedure Set_Return_Value_For_Get_Direction (
      This         : in out Footway_Lane.Mock.Object;
      Return_Value : in     Direction.Straight);

   procedure Set_Return_Value_For_Is_Contained_By (
      This         : in out Footway_Lane.Mock.Object;
      Return_Value : in     Boolean);

   procedure Set_Added_Value_For_Add_Intersection (
      This  : in out Footway_Lane.Mock.Object;
      Added : in     Boolean);

private
   type Return_Values_Collection is record
      Equality_Operator : Boolean;
      Find_Street : Infra_Id;
      Get_Id : Infra_Id;
      Find_Intersections : Infra_Id_Set.Set;
      Append_Stretch : Boolean;
      Count_Stretches : Natural;
      Find_Stretch_Position : Natural;
      Find_Stretch_Position_Found : Boolean;
      Get_Direction : Direction.Straight;
      Is_Contained_By : Boolean;
      Add_Intersection : Boolean;
   end record;

   type Object is
     new Footway_Lane.Object with record
      Return_Values : Return_Values_Collection;
   end record;

   overriding
   procedure Initialize (This : in out Footway_Lane.Mock.Object) is Null;

   overriding
   procedure Finalize (This : in out Footway_Lane.Mock.Object) is Null;

end Reactive.Infrastructure.Lane.Footway_Lane.Mock;
