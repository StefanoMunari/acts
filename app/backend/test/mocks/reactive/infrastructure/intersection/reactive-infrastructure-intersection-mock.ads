with Active.Agent;

package Reactive.Infrastructure.Intersection.Mock is

   package Agent renames Active.Agent;

   type Object (<>) is
     new Intersection.Object
   with private;
   type Reference is access all Intersection.Mock.Object'Class;

   function Create return Intersection.Mock.Reference;

   overriding
   procedure Tread (
      This         : in out Intersection.Mock.Object;
      Traveller_Id : in     Agent.Agent_Id;
      Advanced     :    out Boolean);

   not overriding
   function Is_Connected_At_Direction (This : Intersection.Mock.Object;
                                       Direction : Shared.Direction.Cardinal)
                                       return Boolean;

   overriding
   function Get_Street (This      : in     Intersection.Mock.Object;
                        Direction : in out Shared.Direction.Cardinal)
                        return Infra_Id;

   overriding
   procedure Find_Street_Direction (
      This             :        Intersection.Mock.Object;
      Street_Id        : in     Infra_Id;
      Street_Direction :    out Direction.Cardinal;
      Found            :    out Boolean);

   overriding
   function Find_Streets_Connected_With_Intersection (
      This : in Intersection.Mock.Object) return Infra_Id_Set.Set;

   overriding
   function Get_Id (This : in Intersection.Mock.Object) return Infra_Id;

   overriding
   function Find_Intersections (This : in Intersection.Mock.Object)
                                return Infra_Id_Set.Set;

   overriding
   function Count_Streets (This : in Intersection.Mock.Object) return Natural;

   overriding
   function Exists_Street_For_Direction (
      This      : in Intersection.Mock.Object;
      Direction : in Shared.Direction.Cardinal)
      return Boolean;

   overriding
   function "="(This, Other : Intersection.Mock.Object) return Boolean;

   overriding
   procedure Initialize (This : in out Intersection.Mock.Object);

   overriding
   function Is_Contained_By (This         : in Intersection.Mock.Object;
                             Container_Id : in Infra_Id)
                             return Boolean;

   function Is_Fully_Connected (This : in out Intersection.Mock.Object)
                                return Boolean;

   function Is_Not_Fully_Connected (This : in out Intersection.Mock.Object)
                                    return Boolean;

   procedure Connect_Street (This      : in out Intersection.Mock.Object;
                             Street_Id : in     Infra_Id;
                             Stretches : in     Infra_Id_List.List;
                             Direction : in     Shared.Direction.Cardinal);

   procedure Increment_Streets (This : in out Intersection.Mock.Object);

   procedure Set_Return_Value_For_Tread (
      This     : in out Intersection.Mock.Object;
      Advanced : in Boolean);

   procedure Set_Street (
      This         : in out Intersection.Mock.Object;
      Direction    : in     Shared.Direction.Cardinal;
      Return_Value : in     Infra_Id);

   procedure Set_Return_Value_For_Find_Street_Direction (
      This      : in out Intersection.Mock.Object;
      Direction : in Shared.Direction.Cardinal;
      Found : in Boolean);

   procedure Set_Return_Value_For_Find_Streets_Connected_With_Intersection (
      This         : in out Intersection.Mock.Object;
      Return_Value : in Infra_Id_Set.Set);

   procedure Set_Id (
      This : in out Intersection.Mock.Object;
      Id : in Infra_Id);

   procedure Set_Intersection_Type (
      This              : in out Intersection.Mock.Object;
      Intersection_Type : in Intersection.Intersection_Type);

   procedure Set_Fully_Connected (
      This : in out Intersection.Mock.Object;
      Fully_Connected : in Boolean);

   procedure Set_Return_Value_For_Find_Intersections (
      This         : in out Intersection.Mock.Object;
      Return_Value : in Infra_Id_Set.Set);

   procedure Set_Return_Value_For_Count_Streets (
      This         : in out Intersection.Mock.Object;
      Return_Value : in Natural);

   procedure Set_Return_Value_For_Exists_Street_For_Direction (
      This         : in out Intersection.Mock.Object;
      Return_Value : in Boolean);

   procedure Set_Return_Value_For_Equality_Operator (
      This         : in out Intersection.Mock.Object;
      Return_Value : in Boolean);

   procedure Set_Return_Value_For_Is_Contained_By (
      This         : in out Intersection.Mock.Object;
      Return_Value : in Boolean);

private
   type Street_Map is array (Direction.Cardinal) of Infra_Id;

   type Street_Existence_Map is array (Direction.Cardinal) of Boolean;

   type Mock_Values_Collection is record
      Streets : Street_Map;
      Streets_Existence : Street_Existence_Map;
      Id : Infra_Id;
      Id_Existence : Boolean := FALSE;
      Streets_Count : Natural;
      Streets_Count_Existence : Boolean := FALSE;
      Intersection_Type : Intersection.Intersection_Type;
      Intersection_Type_Existence : Boolean := FALSE;
      Fully_Connected : Boolean;
      Fully_Connected_Existence : Boolean := FALSE;
   end record;

   type Return_Values_Collection is record
      Tread : Boolean;
      Tread_Existence : Boolean := FALSE;
      Find_Street_Direction : Direction.Cardinal;
      Find_Street_Direction_Found : Boolean;
      Find_Street_Direction_Existence : Boolean := FALSE;
      Find_Streets_Connected_With_Intersection : Infra_Id_Set.Set;
      Find_Streets_Connected_With_Intersection_Existence : Boolean := FALSE;
      Find_Intersections : Infra_Id_Set.Set;
      Find_Intersections_Existence : Boolean := FALSE;
      Exists_Street_For_Direction : Boolean;
      Exists_Street_For_Direction_Existence : Boolean := FALSE;
      Equality_Operator : Boolean;
      Equality_Operator_Existence : Boolean := FALSE;
      Is_Contained_By : Boolean;
      Is_Contained_By_Existence : Boolean := FALSE;
   end record;

   type Object is
     new Intersection.Object
   with record
      Mock_Values : Mock_Values_Collection;
      Return_Values : Return_Values_Collection;
   end record;

end Reactive.Infrastructure.Intersection.Mock;
