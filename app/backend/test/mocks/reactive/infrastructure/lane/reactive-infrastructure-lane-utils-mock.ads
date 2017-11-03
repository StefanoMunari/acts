with Active.Agent;

with Shared.Infra_Id_To_Boolean_Map;
with Shared.Infra_Id_To_Direction_Map;
with Shared.Infra_Id_To_Natural_Map;

package Reactive.Infrastructure.Lane.Utils.Mock is

   package Agent renames Active.Agent;
   package Infra_Id_To_Boolean_Map renames Shared.Infra_Id_To_Boolean_Map;
   package Infra_Id_To_Direction_Map renames Shared.Infra_Id_To_Direction_Map;
   package Infra_Id_To_Natural_Map renames Shared.Infra_Id_To_Natural_Map;

   type Object (<>) is new Lane.Utils.Object with private;
   type Reference is access all Lane.Utils.Mock.Object'Class;

   function Create return Lane.Utils.Mock.Reference;

   overriding
   procedure Enter (
      This         : in     Lane.Utils.Mock.Object;
      Lane_Id      : in     Infra_Id;
      Traveller_Id : in     Agent.Agent_Id);

   overriding
   function Find_Street (This    : in Lane.Utils.Mock.Object;
                         Lane_Id : in Infra_Id)
   return Infra_Id;

   overriding
   function Find_Intersections (This    : in Lane.Utils.Mock.Object;
                                Lane_Id : in Infra_Id)
   return Infra_Id_Set.Set;

   overriding
   function Count_Stretches (This    : in Lane.Utils.Mock.Object;
                             Lane_Id : in Infra_Id) return Natural;

   overriding
   procedure Find_Stretch_Position (
      This                : in     Lane.Utils.Mock.Object;
      Lane_Id, Stretch_Id : in     Infra_Id;
      Stretch_Position    :    out Natural;
      Found               :    out Boolean);

   overriding
   function Get_Direction (This : in Lane.Utils.Mock.Object;
                           Lane_Id : in Infra_Id)
                           return Direction.Straight;

   overriding
   procedure Add_Intersection (
      This            : in     Lane.Utils.Mock.Object;
      Lane_Id,
      Intersection_Id : in     Infra_Id;
      Added           :    out Boolean);

   overriding
   function Is_Contained_By (This                  : in Lane.Utils.Mock.Object;
                             Lane_Id, Container_Id : in Infra_Id)
                             return Boolean;

   not overriding
   procedure Set_Return_Value_For_Find_Street (
      This         : in out Lane.Utils.Mock.Object;
      Return_Value : in     Infra_Id);

   not overriding
   procedure Set_Return_Value_For_Find_Intersections (
      This         : in out Lane.Utils.Mock.Object;
      Return_Value : in     Infra_Id_Set.Set);

   not overriding
   procedure Set_Return_Value_For_Count_Stretches (
      This         : in out Lane.Utils.Mock.Object;
      Return_Value : in     Natural);

   not overriding
   procedure Set_Return_Value_For_Find_Stretch_Position (
      This         : in out Lane.Utils.Mock.Object;
      Stretch_Id   : in     Infra_Id;
      Return_Value : in     Natural;
      Found        : in     Boolean);

   not overriding
   procedure Set_Return_Value_For_Get_Direction (
      This      : in out Lane.Utils.Mock.Object;
      Lane_Id   : in     Infra_Id;
      Direction : in     Shared.Direction.Straight);

   not overriding
   procedure Set_Return_Value_For_Add_Intersection (
      This         : in out Lane.Utils.Mock.Object;
      Return_Value : in     Boolean);

   not overriding
   procedure Set_Return_Value_For_Is_Contained_By (
      This         : in out Lane.Utils.Mock.Object;
      Return_Value : in     Boolean);

private
   type Return_Values_Collection is record
      Find_Street : Infra_Id;
      Find_Street_Existence : Boolean := FALSE;
      Find_Intersections : Infra_Id_Set.Set;
      Find_Intersections_Existence : Boolean := FALSE;
      Count_Stretches : Natural;
      Count_Stretches_Existence : Boolean := FALSE;
      Find_Stretch_Position : Infra_Id_To_Natural_Map.Map;
      Find_Stretch_Position_Found : Infra_Id_To_Boolean_Map.Map;
      Get_Direction : Infra_Id_To_Direction_Map.Map;
      Add_Intersection : Boolean;
      Add_Intersection_Existence : Boolean := FALSE;
      Is_Contained_By : Boolean;
      Is_Contained_By_Existence : Boolean := FALSE;
   end record;

   type Object is new Lane.Utils.Object with record
      Return_Values : Return_Values_Collection;
   end record;

end Reactive.Infrastructure.Lane.Utils.Mock;
