with Ada.Containers.Ordered_Maps;

with Active.Agent;
with Active.Traveller.Mock;

limited with Reactive.District.Mock;

with Shared.Agent_Id_List;
with Shared.Infra_Id_To_Boolean_Map;
with Shared.Agent_Id_To_Infra_Id_Map;

package Active.Traveller.Utils.Mock is

   package Agent renames Active.Agent;
   package Agent_Id_List renames Shared.Agent_Id_List;
   package Infra_Id_To_Boolean_Map renames Shared.Infra_Id_To_Boolean_Map;
   package Agent_Id_To_Infra_Id_Map renames Shared.Agent_Id_To_Infra_Id_Map;

   package Agent_Id_To_Slice_Map is
    new Ada.Containers.Ordered_Maps (
      Key_Type        => Active.Agent.Agent_Id,
      Element_Type    => Slice.Map,
      "="             => Slice."=",
      "<"             => Active.Agent."<");

   type Object (<>) is new Traveller.Utils.Object with private;
   type Reference is access all Traveller.Utils.Mock.Object'Class;

   function Create return Traveller.Utils.Mock.Reference;

   overriding
   function Get_Stretch_Type (
      This         : in Traveller.Utils.Mock.Object;
      Traveller_Id : in Agent.Agent_Id)
   return Stretch_Type;

   overriding
   procedure Consume_Step (
      This         : in out Traveller.Utils.Mock.Object;
      Traveller_Id : in     Agent.Agent_Id);

   overriding
   function Get_Next_Step (This         : in     Traveller.Utils.Mock.Object;
                           Traveller_Id : in     Agent.Agent_Id)
   return Infra_Id;

   overriding
   function Get_Position (This         : in     Traveller.Utils.Mock.Object;
                          Traveller_Id : in     Agent.Agent_Id)
   return Infra_Id;

   overriding
   procedure Set_Position (This         : in     Traveller.Utils.Mock.Object;
                           Traveller_Id : in     Agent.Agent_Id;
                           New_Position : in     Infra_Id) is null;

   overriding
   procedure Set_Current_Speed (This         : in     Traveller.Utils.Mock.Object;
                                Traveller_Id : in     Agent.Agent_Id;
                                New_Speed    : in     Natural);

   overriding
   function Get_Current_Speed (
      This         : in     Traveller.Utils.Mock.Object;
      Traveller_Id : in     Agent.Agent_Id)
   return Natural;

   overriding
   procedure Set_Travel (This         : in out Mock.Object;
                         Traveller_Id : in     Agent.Agent_Id;
                         Travel       : access Active.Travel.Object'Class)
   is null;

   overriding
   function Get_Travel_Source (This         : in     Mock.Object;
                               Traveller_Id : in     Agent.Agent_Id)
   return Slice.Map;

   overriding
   function Get_Travel_Destination (This         : in     Mock.Object;
                                    Traveller_Id : in     Agent.Agent_Id)
   return Slice.Map;

   overriding
   function Does_Travel_Contain_Step (
      This         : in Traveller.Utils.Mock.Object;
      Traveller_Id : in     Agent.Agent_Id;
      Step         : in Infra_Id)
   return Boolean;

   overriding
   function Does_Travel_Contain_Steps (
      This         : in     Traveller.Utils.Mock.Object;
      Traveller_Id : in     Agent.Agent_Id;
      Step         : in     Slice.Map)
   return Boolean;

   overriding
   procedure Erase_Route (
      This         : in out Traveller.Utils.Mock.Object;
      Traveller_Id :        Agent.Agent_Id) is null;

   overriding
   function Get_List_From_Slice (This         : in     Mock.Object;
                                 Traveller_Id : in     Agent.Agent_Id;
                                 Slice_Obj    : in     Slice.Map)
   return Infra_Id_List.List;

   overriding
   procedure Defer (This         : in out Traveller.Utils.Mock.Object;
                    Traveller_Id : in     Agent.Agent_Id;
                    Retry_Action : in     Boolean);

   overriding
   function Get_Size (
      This         : in Traveller.Utils.Mock.Object;
      Traveller_Id : in Agent.Agent_Id)
   return Natural;

   not overriding
   procedure Set_Return_Value_For_Get_Stretch_Type (
      This         : in out Traveller.Utils.Mock.Object;
      Return_Value : in     Stretch_Type);

   not overriding
   procedure Set_Return_Value_For_Get_Next_Step (
      This         : in out Traveller.Utils.Mock.Object;
      Traveller_Id : in     Agent.Agent_Id;
      Return_Value : in     Infra_Id);

   not overriding
   procedure Set_Return_Value_For_Get_Position (
      This         : in out Traveller.Utils.Mock.Object;
      Return_Value : in     Infra_Id);

   not overriding
   procedure Set_Traveller_For_Set_Current_Speed (
      This          : in out Traveller.Utils.Mock.Object;
      Traveller_Ref : in     Active.Traveller.Mock.Reference);

   not overriding
   procedure Set_Return_Value_For_Get_Travel_Source (
      This         : in out Traveller.Utils.Mock.Object;
      Traveller_Id : in     Agent.Agent_Id;
      Source       : in     Slice.Map);

   not overriding
   procedure Set_Return_Value_For_Get_Travel_Destination (
      This         : in out Traveller.Utils.Mock.Object;
      Traveller_Id : in     Agent.Agent_Id;
      Destination  : in     Slice.Map);

   not overriding
   procedure Set_Return_Value_For_Does_Travel_Contain_Step (
      This         : in out Traveller.Utils.Mock.Object;
      Stretch_Id   : in     Infra_Id;
      Return_Value : in     Boolean);

   not overriding
   procedure Set_Return_Value_For_Does_Travel_Contain_Steps (
      This         : in out Traveller.Utils.Mock.Object;
      Traveller_Id : in     Agent.Agent_Id);

   not overriding
   procedure Set_Return_Value_For_Get_List_From_Slice (
      This     : in out Traveller.Utils.Mock.Object;
      The_List : in     Infra_Id_List.List);

   not overriding
   procedure Set_Return_Value_For_Get_Size (
      This : in out Traveller.Utils.Mock.Object;
      Size : in     Natural);

   not overriding
   function Get_Consume_Step_Called (
      This : in out Traveller.Utils.Mock.Object)
   return Boolean;

private
   type Mock_Values_Collection is record
      Consume_Step_Called : Boolean := False;
   end record;

   type Return_Values_Collection is record
      Get_Stretch_Type : Stretch_Type;
      Get_Stretch_Type_Existence : Boolean := False;
      Get_Next_Step : Agent_Id_To_Infra_Id_Map.Map;
      Get_Position : Infra_Id;
      Get_Position_Existence : Boolean := False;
      Traveller_Ref_Speed : Active.Traveller.Mock.Reference;
      Traveller_Ref_Speed_Existence : Boolean := False;
      Does_Travel_Contain_Step : Infra_Id_To_Boolean_Map.Map;
      Does_Travel_Contain_Steps : Agent_Id_List.List;
      Does_Travel_Contain_Steps_Existence : Boolean := False;
      Get_List_From_Slice : Infra_Id_List.List;
      Get_List_From_Slice_Existence : Boolean := False;
      Get_Travel_Destination : Agent_Id_To_Slice_Map.Map;
      Get_Travel_Source : Agent_Id_To_Slice_Map.Map;
      Get_Size : Natural;
      Get_Size_Existence : Boolean := False;
   end record;

   type Object is new Traveller.Utils.Object with record
      Return_Values : Return_Values_Collection;
      Mock_Values   : Mock_Values_Collection;
   end record;

end Active.Traveller.Utils.Mock;
