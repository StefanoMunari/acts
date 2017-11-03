with Active.Agent;

package Reactive.Infrastructure.Lane.Mock is

   package Agent renames Active.Agent;

   type Object is new Lane.Object with private;
   type Reference is access all Lane.Mock.Object'Class;

   function Create return Lane.Mock.Reference;

   overriding
   procedure Enter (
      This         : in out Lane.Mock.Object;
      Traveller_Id : in     Agent.Agent_Id);

   overriding
   function "=" (This, Outher : in Lane.Mock.Object) return Boolean;

   overriding
   function Find_Street (This : in Lane.Mock.Object)
                         return Infra_Id;

   overriding
   function Get_Id (This : in Lane.Mock.Object) return Infra_Id;

   overriding
   function Find_Intersections (This : in Lane.Mock.Object)
                                return Infra_Id_Set.Set;

   overriding
   procedure Append_Stretch (
      This       : in out Lane.Mock.Object;
      Stretch_Id : in     Infra_Id;
      Added      :    out Boolean);

   overriding
   function Count_Stretches (This : in Lane.Mock.Object) return Natural;

   overriding
   procedure Find_Stretch_Position (
      This             : in     Lane.Mock.Object;
      Stretch_Id       : in     Infra_Id;
      Stretch_Position :    out Natural;
      Found            :    out Boolean);

   overriding
   function Get_Direction (This : Lane.Mock.Object)
                           return Direction.Straight;

   overriding
   function Is_Contained_By (This         : in Lane.Mock.Object;
                             Container_Id : in Infra_Id) return Boolean;

   overriding
   procedure Set_Way (This : in out Lane.Mock.Object; Way_Id : in Infra_Id)
   is null;

   overriding
   procedure Add_Intersection (
      This            : in out Lane.Mock.Object;
      Intersection_Id : in     Infra_Id;
      Added           :    out Boolean);

   procedure Set_Return_Value_For_Equality_Operator (
      This         : in out Lane.Mock.Object;
      Return_Value : in     Boolean);

   procedure Set_Return_Value_For_Find_Street (
      This         : in out Lane.Mock.Object;
      Return_Value : in     Infra_Id);

   procedure Set_Id (
      This         : in out Lane.Mock.Object;
      Return_Value : in     Infra_Id);

   procedure Set_Return_Value_For_Find_Intersections (
      This         : in out Lane.Mock.Object;
      Return_Value : in     Infra_Id_Set.Set);

   procedure Set_Return_Value_For_Append_Stretch (
      This  : in out Lane.Mock.Object;
      Added : in     Boolean);

   procedure Set_Return_Value_For_Count_Stretches (
      This         : in out Lane.Mock.Object;
      Return_Value : in     Natural);

   procedure Set_Return_Value_For_Find_Stretch_Position (
      This     : in out Lane.Mock.Object;
      Position : in     Natural;
      Found    : in     Boolean);

   procedure Set_Direction (
      This         : in out Lane.Mock.Object;
      Return_Value : in     Direction.Straight);

   procedure Set_Return_Value_For_Is_Contained_By (
      This         : in out Lane.Mock.Object;
      Return_Value : in     Boolean);

   procedure Set_Return_Value_For_Add_Intersection (
      This  : in out Lane.Mock.Object;
      Added : in     Boolean);

private
   type Mock_Values_Collection is record
      Id : Infra_Id;
      Id_Existence : Boolean := FALSE;
      Way_Id : Infra_Id;
      Way_Id_Existence : Boolean := FALSE;
      Direction : Shared.Direction.Straight;
      Direction_Existence : Boolean := FALSE;
   end record;

   type Return_Values_Collection is record
      Equality_Operator : Boolean;
      Equality_Operator_Existence : Boolean := FALSE;
      Find_Street : Infra_Id;
      Find_Street_Existence : Boolean := FALSE;
      Find_Intersections : Infra_Id_Set.Set;
      Find_Intersections_Existence : Boolean := FALSE;
      Append_Stretch : Boolean;
      Append_Stretch_Existence : Boolean := FALSE;
      Count_Stretches : Natural;
      Count_Stretches_Existence : Boolean := FALSE;
      Find_Stretch_Position : Natural;
      Find_Stretch_Position_Found : Boolean;
      Find_Stretch_Position_Existence : Boolean := FALSE;
      Is_Contained_By : Boolean;
      Is_Contained_By_Existence : Boolean := FALSE;
      Add_Intersection : Boolean;
      Add_Intersection_Existence : Boolean := FALSE;
   end record;

   type Object is
     new Lane.Object with record
      Mock_Values : Mock_Values_Collection;
      Return_Values : Return_Values_Collection;
   end record;

   overriding
   procedure Initialize (This : in out Lane.Mock.Object) is Null;

   overriding
   procedure Finalize (This : in out Lane.Mock.Object) is Null;

end Reactive.Infrastructure.Lane.Mock;
