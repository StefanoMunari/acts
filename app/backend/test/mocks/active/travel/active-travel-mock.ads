with Active.Agent;

with Shared.Slice;
with Shared.Infra_Id_To_Boolean_Map;

package Active.Travel.Mock is

   package Agent renames Active.Agent;
   package Slice renames Shared.Slice;
   package Infra_Id_To_Boolean_Map renames Shared.Infra_Id_To_Boolean_Map;

   type Object (<>) is new Active.Travel.Object with private;
   type Reference is access all Travel.Mock.Object'Class;

   function Create return Travel.Mock.Reference;

   overriding
   procedure Advance (This : in out Travel.Mock.Object);

   overriding
   function Has_Next_Step (This : in Travel.Mock.Object) return Boolean;

   overriding
   function Is_Progressing (This : in Travel.Mock.Object) return Boolean;

   overriding
   function Get_Route_Source (This : in Travel.Mock.Object)
   return Slice.Map;

   overriding
   function Get_Route_Destination (This : in Travel.Mock.Object)
   return Slice.Map;

   overriding
   function Get_Traveller_Id (This : in Travel.Mock.Object)
   return Agent.Agent_Id;

   overriding
   function Get_Current_Step_Id (This : in Travel.Mock.Object)
   return Infra_Id;

   overriding
   function Get_Next_Step_Id (This : in Travel.Mock.Object)
   return Infra_Id;

   overriding
   function Get_Previous_Step_Id (This : in Travel.Mock.Object)
   return Infra_Id;

   overriding
   function Contains (This :    Travel.Mock.Object;
                      Step : in Infra_Id)
   return Boolean;

   overriding
   function Contains (This        : Travel.Mock.Object;
                      Steps_Slice : in Slice.Map)
   return Boolean;

   not overriding
   procedure Set_Return_Value_For_Has_Next_Step (
      This         : in out Travel.Mock.Object;
      Return_Value : in Boolean);

   not overriding
   procedure Set_Return_Value_For_Is_Progressing (
      This         : in out Travel.Mock.Object;
      Return_Value : in Boolean);

   not overriding
   procedure Set_Return_Value_For_Get_Route_Source (
      This         : in out Travel.Mock.Object;
      Return_Value : in     Slice.Map);

   not overriding
   procedure Set_Return_Value_For_Get_Route_Destination (
      This         : in out Travel.Mock.Object;
      Return_Value : in     Slice.Map);

   not overriding
   procedure Set_Return_Value_For_Get_Traveller_Id (
      This         : in out Travel.Mock.Object;
      Return_Value : in     Agent.Agent_Id);

   not overriding
   procedure Set_Return_Value_For_Get_Current_Step_Id (
      This         : in out Travel.Mock.Object;
      Return_Value : in     Infra_Id);

   not overriding
   procedure Set_Return_Value_For_Get_Next_Step (
      This         : in out Travel.Mock.Object;
      Return_Value : in     Infra_Id);

   not overriding
   procedure Set_Return_Value_For_Get_Previous_Step (
      This         : in out Travel.Mock.Object;
      Return_Value : in     Infra_Id);

   not overriding
   procedure Set_Return_Value_For_Get_First_Step_Id (
      This         : in out Travel.Mock.Object;
      Return_Value : in     Infra_Id);

   not overriding
   procedure Set_Return_Value_For_Get_Last_Step_Id (
      This         : in out Travel.Mock.Object;
      Return_Value : in     Infra_Id);

   not overriding
   procedure Set_Return_Value_For_Contains (
      This         : in out Travel.Mock.Object;
      Stretch_Id   : in     Infra_Id;
      Return_Value : in     Boolean);

   not overriding
   procedure Set_Return_Value_For_Contains_Slice (
      This         : in out Travel.Mock.Object;
      Return_Value : in     Boolean);

   not overriding
   function Get_Advance_Called (This : in out Travel.Mock.Object)
   return Boolean;

   not overriding
   function Get_Travel_State (
      This : in Travel.Mock.Object)
      return access Travel.Travel_State.Object'Class;

   procedure Change_Travel_State (
      This         : in out Travel.Mock.Object;
      Travel_State : access Travel.Travel_State.Object'Class);

   procedure Clear_Route (
      This       : in out Travel.Mock.Object;
      Clean      : out Boolean);

   procedure Prepend_Step (
      This    : in out Travel.Mock.Object;
      Step_Id : in     Infra_Id);

   function Get_First_Step_Id (This : Travel.Mock.Object)
   return Infra_Id;

   function Get_Last_Step_Id (This : Travel.Mock.Object)
   return Infra_Id;

   function Get_Route (This : in Travel.Mock.Object) return Infra_Id_List.List;

private
   type Return_Values_Collection is record
      Has_Next_Step : Boolean;
      Has_Next_Step_Existence : Boolean := FALSE;
      Is_Progressing : Boolean;
      Is_Progressing_Existence : Boolean := FALSE;
      Get_Route_Source : Slice.Map;
      Get_Route_Source_Existence : Boolean := FALSE;
      Get_Route_Destination : Slice.Map;
      Get_Route_Destination_Existence : Boolean := FALSE;
      Get_Traveller_Id : Agent.Agent_Id;
      Get_Traveller_Id_Existence : Boolean := FALSE;
      Get_Current_Step_Id : Infra_Id;
      Get_Current_Step_Id_Existence : Boolean := FALSE;
      Get_Next_Step_Id : Infra_Id;
      Get_Next_Step_Id_Existence : Boolean := FALSE;
      Get_Previous_Step_Id : Infra_Id;
      Get_Previous_Step_Id_Existence : Boolean := FALSE;
      Travel_State : access Travel.Travel_State.Object'Class := null;
      Route : Infra_Id_List.List;
      Get_Current_Step : Infra_Id_List.Cursor;
      Get_Current_Step_Existence : Boolean := FALSE;
      Get_First_Step_Id : Infra_Id;
      Get_First_Step_Id_Existence : Boolean := FALSE;
      Get_Last_Step_Id : Infra_Id;
      Get_Last_Step_Id_Existence : Boolean := FALSE;
      Contains : Infra_Id_To_Boolean_Map.Map;
      Contains_Slice : Boolean;
      Contains_Slice_Existence : Boolean := FALSE;
   end record;

   type Mock_Values_Collection is record
      Advance_Called : Boolean := False;
   end record;

   type Object is
     new Travel.Object
   with record
      Return_Values : Return_Values_Collection;
      Mock_Values   : Mock_Values_Collection;
   end record;

   overriding
   procedure Initialize (This : in out Travel.Mock.Object) is null;

end Active.Travel.Mock;
