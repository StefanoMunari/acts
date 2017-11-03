with Active.Agent;

with Shared.Infra_Id_To_Boolean_Map;

package Active.Traveller.Mock is

   package Agent renames Active.Agent;
   package Infra_Id_To_Boolean_Map renames Shared.Infra_Id_To_Boolean_Map;

   type Object is
     new Traveller.Object
   with private;
   type Reference is access all Traveller.Mock.Object'Class;

   function Create return Traveller.Mock.Reference;

   overriding
   function Get_Id (This : in Traveller.Mock.Object) return Agent.Agent_Id;

   overriding
   function Get_Stretch_Type (This : in Traveller.Mock.Object)
   return Stretch_Type;

   overriding
   function Get_Maximum_Speed (This : in Traveller.Mock.Object)
   return Natural;

   overriding
   function Get_Current_Speed (This : in Traveller.Mock.Object)
   return Natural;

   overriding
   procedure Set_Current_Speed (This      : in out Traveller.Mock.Object;
                                New_Speed : in     Natural);

   overriding
   procedure Act (This : in out Traveller.Mock.Object) is null;

   overriding
   procedure Travel (This : in out Traveller.Mock.Object) is null;

   overriding
   function Has_Next_Step (This : in Traveller.Mock.Object) return Boolean;

   overriding
   function Is_Travelling (This : in Traveller.Mock.Object) return Boolean;

   overriding
   function "=" (This, Other : Traveller.Mock.Object) return Boolean;

   overriding
   function Get_List_From_Slice (This      : in Traveller.Mock.Object;
                                 The_Slice : in Slice.Map)
   return Infra_Id_List.List;

   function Wait_For_Bus (This : in Traveller.Mock.Object)
                          return Boolean;

   overriding
   function Is_Affected_By_Traffic_Lights (This : in Traveller.Mock.Object)
   return Boolean;

   overriding
   function Get_Size (This : in Traveller.Mock.Object)
   return Natural;

   not overriding
   procedure Set_Id (
      This  : in out Traveller.Mock.Object;
      Id    : in     Agent.Agent_Id);

   not overriding
   procedure Set_Return_Value_For_Get_Stretch_Type (
      This         : in out Traveller.Mock.Object;
      Return_Value : in     Stretch_Type);

   not overriding
   procedure Set_Maximum_Speed (
      This          : in out Traveller.Mock.Object;
      Maximum_Speed : in Natural);

   not overriding
   procedure Set_Return_Value_For_Has_Next_Step (
      This         : in out Traveller.Mock.Object;
      Return_Value : in Boolean);

   not overriding
   procedure Set_Return_Value_For_Is_Travelling (
      This         : in out Traveller.Mock.Object;
      Return_Value : in Boolean);

   not overriding
   procedure Set_Return_Value_For_Equality_Operator (
      This         : in out Traveller.Mock.Object;
      Return_Value : in     Boolean);

   not overriding
   procedure Set_Return_Value_For_Tread_Street (
      This         : in out Traveller.Mock.Object;
      Street_Id    : in     Infra_Id;
      Return_Value : in     Boolean);

   not overriding
   procedure Set_List_Value_For_Get_List_From_Slice (
      This     : in out Traveller.Mock.Object;
      The_List : in     Infra_Id_List.List);

   not overriding
   procedure Set_Return_Value_For_Wait_For_Bus (
      This         : in out Traveller.Mock.Object;
      Return_Value : in     Boolean);

   not overriding
   procedure Set_Return_Value_For_Is_Affected_By_Traffic_Lights (
      This         : in out Traveller.Mock.Object;
      Return_Value : in     Boolean);

   not overriding
   procedure Set_Return_Value_For_Get_Size (
      This         : in out Traveller.Mock.Object;
      Return_Value : in     Natural);

private
   type Mock_Values_Collection is record
      Id : Agent.Agent_Id;
      Id_Existence : Boolean := FALSE;
      Maximum_Speed : Natural;
      Maximum_Speed_Existence : Boolean := FALSE;
      Current_Speed : Natural;
      Current_Speed_Existence : Boolean := FALSE;
   end record;

   type Return_Values_Collection is record
      Get_Stretch_Type : Stretch_Type;
      Get_Stretch_Type_Existence : Boolean := FALSE;
      Has_Next_Step : Boolean;
      Is_Travelling : Boolean;
      Is_Travelling_Existence : Boolean := FALSE;
      Equality_Operator : Boolean;
      Tread_Street : Infra_Id_To_Boolean_Map.Map;
      The_List : Infra_Id_List.List;
      The_List_Existence : Boolean := FALSE;
      Wait_For_Bus : Boolean;
      Affected_By_Traffic_Lights : Boolean;
      Affected_By_Traffic_Lights_Existence : Boolean := FALSE;
      Get_Size : Natural;
      Get_Size_Existence : Boolean := FALSE;
   end record;

   type Object is
     new Traveller.Object
   with record
      Mock_Values : Mock_Values_Collection;
      Return_Values : Return_Values_Collection;
   end record;

end Active.Traveller.Mock;
