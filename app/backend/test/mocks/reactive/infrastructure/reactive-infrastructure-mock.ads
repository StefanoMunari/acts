with Active.Agent;

with Shared.Infra_Id_Set;
with Shared.Infra_Id_To_Boolean_Map;
with Shared.Direction;

package Reactive.Infrastructure.Mock is

   package Agent renames Active.Agent;
   package Direction renames Shared.Direction;
   package Infra_Id_Set renames Shared.Infra_Id_Set;
   package Infra_Id_To_Boolean_Map renames Shared.Infra_Id_To_Boolean_Map;
   use Reactive.Infra_Id_Type;

   type Object (<>) is
     new Infrastructure.Object
   with private;
   type Reference is access all Infrastructure.Mock.Object'Class;

   function Create return Infrastructure.Mock.Reference;

   overriding
   function Get_Id (This : in Infrastructure.Mock.Object) return Infra_Id;

   overriding
   function "=" (This, Outher : in Infrastructure.Mock.Object) return Boolean;

   overriding
   function Is_Contained_By (This         : in Infrastructure.Mock.Object;
                             Container_Id : in Infra_Id) return Boolean;

   overriding
   function Dump (This : Infrastructure.Mock.Object)
   return G_JSON.JSON_Value;

   not overriding
   procedure Set_Id
     (This         : in out Infrastructure.Mock.Object;
      Return_Value : in     Infra_Id);

   not overriding
   procedure Set_Return_Value_For_Equality_Operator
     (This         : in out Infrastructure.Mock.Object;
      Return_Value : in     Boolean);

   not overriding
   procedure Set_Return_Value_For_Is_Contained_By
     (This         : in out Infrastructure.Mock.Object;
      Container_Id : in     Infra_Id;
      Return_Value : in     Boolean);

   not overriding
   procedure Set_Return_Value_For_Dump (
      This         : in out Infrastructure.Mock.Object;
      Return_Value : in     G_JSON.JSON_Value);

private
   type Mock_Values_Collection is record
      Id : Infra_Id;
      Id_Existence : Boolean := FALSE;
      Orientation : Direction.Orientation;
      Orientation_Existence : Boolean := FALSE;
   end record;

   type Return_Values_Collection is record
      Is_Treadable_In_Direction : Boolean;
      Is_Treadable_In_Direction_Existence : Boolean := FALSE;
      Is_Not_Treadable_In_Direction : Boolean;
      Is_Not_Treadable_In_Direction_Existence : Boolean := FALSE;
      Tread_Footway : Boolean;
      Tread_Footway_Existence : Boolean := FALSE;
      Tread_Bikeway : Boolean;
      Tread_Bikeway_Existence : Boolean := FALSE;
      Tread_Roadway : Boolean;
      Tread_Roadway_Existence : Boolean := FALSE;
      Equality_Operator : Boolean;
      Equality_Operator_Existence : Boolean := FALSE;
      Find_Infrastructure : Infra_Id;
      Find_Infrastructure_Existence : Boolean := FALSE;
      Find_Lanes_By_Direction : Infra_Id_Set.Set;
      Find_Lanes_By_Direction_Existence : Boolean := FALSE;
      Is_Contained_By : Infra_Id_To_Boolean_Map.Map;
      Is_Contained_By_Existence : Boolean := FALSE;
      Add_Intersection : Boolean;
      Add_Intersection_Existence : Boolean := FALSE;
      Dump : G_JSON.JSON_Value;
      Dump_Existence : Boolean := FALSE;
   end record;

   type Object is
     new Infrastructure.Object
   with record
      Mock_Values : Mock_Values_Collection;
      Return_Values : Return_Values_Collection;
   end record;

end Reactive.Infrastructure.Mock;
