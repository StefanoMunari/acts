with Active.Agent;

with Shared.Infra_Id_To_Boolean_Map;
with Shared.Infra_Id_List;
with Shared.Direction;

package Reactive.Infrastructure.Street.Mock is

   package Agent renames Active.Agent;
   package Direction renames Shared.Direction;
   package Infra_Id_List renames Shared.Infra_Id_List;
   package Infra_Id_To_Boolean_Map renames Shared.Infra_Id_To_Boolean_Map;

   type Object (<>) is
     new Street.Object
   with private;
   type Reference is access all Street.Mock.Object'Class;

   function Create return Street.Mock.Reference;

   overriding
   function Is_Treadable_In_Direction (
      This      : in Street.Mock.Object;
      Direction : in Shared.Direction.Cardinal)
   return Boolean;

   overriding
   function Is_Not_Treadable_In_Direction (
      This      : in Street.Mock.Object;
      Direction : in Shared.Direction.Cardinal)
   return Boolean;

   overriding
   function Get_Id (This : in Street.Mock.Object) return Infra_Id;

   overriding
   function Get_Orientation (This: in Street.Mock.Object)
                             return Direction.Orientation;

   overriding
   procedure Add_Roadway (This       :    out Street.Mock.Object;
                          Roadway_Id : in     Infra_Id) is null;

   overriding
   procedure Add_Footway (This       : out Street.Mock.Object;
                          Footway_Id : in Infra_Id) is null;

   overriding
   procedure Add_Bikeway (This       : out Street.Mock.Object;
                          Bikeway_Id : in Infra_Id) is null;

   overriding
   function "=" (This, Outher : in Street.Mock.Object) return Boolean;

   overriding
   function Find_Street (This : in Street.Mock.Object)
                         return Infra_Id;

   overriding
   function Find_Lanes_By_Direction (This             : in Street.Mock.Object;
                                     Travel_Direction : in Direction.Straight)
                                     return Infra_Id_Set.Set;

   overriding
   function Is_Contained_By (This         : in Street.Mock.Object;
                             Container_Id : in Infra_Id) return Boolean;

   not overriding
   procedure Set_Return_Value_For_Is_Treadable_In_Direction (
      This         : in out Street.Mock.Object;
      Return_Value : in     Boolean);

   not overriding
   procedure Set_Return_Value_For_Is_Not_Treadable_In_Direction (
      This         : in out Street.Mock.Object;
      Return_Value : in     Boolean);

   not overriding
   procedure Set_Id (
      This         : in out Street.Mock.Object;
      Return_Value : in     Infra_Id);

   not overriding
   procedure Set_Orientation (
      This         : in out Street.Mock.Object;
      Return_Value : in     Direction.Orientation);

   not overriding
   procedure Set_Return_Value_For_Equality_Operator (
      This         : in out Street.Mock.Object;
      Return_Value : in     Boolean);

   not overriding
   procedure Set_Return_Value_For_Find_Street (
      This         : in out Street.Mock.Object;
      Return_Value : in     Infra_Id);

   not overriding
   procedure Set_Return_Value_For_Find_Lanes_By_Direction (
      This         : in out Street.Mock.Object;
      Return_Value : in     Infra_Id_Set.Set);

   not overriding
   procedure Set_Return_Value_For_Is_Contained_By (
      This         : in out Street.Mock.Object;
      Container_Id : in     Infra_Id;
      Return_Value : in     Boolean);

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
      Equality_Operator : Boolean;
      Equality_Operator_Existence : Boolean := FALSE;
      Find_Street : Infra_Id;
      Find_Street_Existence : Boolean := FALSE;
      Find_Lanes_By_Direction : Infra_Id_Set.Set;
      Find_Lanes_By_Direction_Existence : Boolean := FALSE;
      Is_Contained_By : Infra_Id_To_Boolean_Map.Map;
      Is_Contained_By_Existence : Boolean := FALSE;
   end record;

   type Object is
     new Street.Object
   with record
      Mock_Values : Mock_Values_Collection;
      Return_Values : Return_Values_Collection;
   end record;

end Reactive.Infrastructure.Street.Mock;
