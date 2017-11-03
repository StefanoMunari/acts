with Active.Agent;

with Shared.Infra_Id_Map;
with Shared.Infra_Id_Multimap;
with Shared.Infra_Id_To_Boolean_Map;

package Reactive.Infrastructure.Stretch.Utils.Mock is

   package Agent renames Active.Agent;
   package Infra_Id_Map renames Shared.Infra_Id_Map;
   package Infra_Id_Multimap renames Shared.Infra_Id_Multimap;
   package Infra_Id_To_Boolean_Map renames Shared.Infra_Id_To_Boolean_Map;

   type Object (<>) is new Stretch.Utils.Object with private;
   type Reference is access all Stretch.Utils.Mock.Object'Class;

   function Create return Stretch.Utils.Mock.Reference;

   overriding
   function Get_Id (
      This              : in Stretch.Utils.Mock.Object;
      Infrastructure_Id : in Infra_Id)
   return Infra_Id;

   overriding
   procedure Tread (This         : in     Stretch.Utils.Mock.Object;
                    Stretch_Id   : in     Infra_Id;
                    Traveller_Id : in     Agent.Agent_Id;
                    Advanced     :    out Boolean);

   overriding
   function Is_Waiting_To_Enter_Stretch (
      This         : in Stretch.Utils.Mock.Object;
      Stretch_Id   : in Infra_Id;
      Traveller_Id : in Agent.Agent_Id)
   return Boolean;

   overriding
   procedure Leave (
      This         : in     Stretch.Utils.Mock.Object;
      Stretch_Id   : in     Infra_Id;
      Traveller_Id : in     Agent.Agent_Id;
      Left         :    out Boolean);

   overriding
   function Is_Before (
      This  : in Stretch.Utils.Mock.Object;
      Left  : in Infra_Id;
      Right : in Infra_Id)
   return Boolean;

   overriding
   function Get_Host (This       : in Stretch.Utils.Mock.Object;
                      Stretch_Id : in Infra_Id)
   return Infra_Id;

   overriding
   function Has_Host (This       : in Stretch.Utils.Mock.Object;
                      Stretch_Id : in Infra_Id)
   return Boolean;

   overriding
   function Find_Intersections (This : in Stretch.Utils.Mock.Object;
                                Stretch_Id : in Infra_Id)
                                return Infra_Id_Set.Set;

   not overriding
   procedure Set_Return_Value_For_Get_Id (
      This              : in out Stretch.Utils.Mock.Object;
      Infrastructure_Id : in     Infra_Id;
      Stretch_Id        : in     Infra_Id);

   not overriding
   procedure Set_Return_Value_For_Tread (
      This         : in out Stretch.Utils.Mock.Object;
      Return_Value : in Boolean);

   not overriding
   procedure Set_Return_Value_For_Is_Waiting_To_Enter_Stretch (
      This         : in out Stretch.Utils.Mock.Object;
      Return_Value : in Boolean);

   not overriding
   procedure Set_Return_Value_For_Leave (
      This         : in out Stretch.Utils.Mock.Object;
      Return_Value : in Boolean);

   not overriding
   procedure Set_Return_Value_For_Is_Before (
      This         : in out Stretch.Utils.Mock.Object;
      Stretch1_Id, Stretch2_Id : in Infra_Id;
      Return_Value : in Boolean);

   not overriding
   procedure Set_Return_Value_For_Get_Host (
      This         : in out Stretch.Utils.Mock.Object;
      Stretch_Id   : in Infra_Id;
      Return_Value : in Infra_Id);

   not overriding
   procedure Set_Return_Value_For_Has_Host (
      This         : in out Stretch.Utils.Mock.Object;
      Stretch_Id   : in Infra_Id;
      Return_Value : in Boolean);

   not overriding
   procedure Set_Return_Value_For_Find_Intersections (
      This         : in out Stretch.Utils.Mock.Object;
      Stretch_Id   : in Infra_Id;
      Return_Value : in Infra_Id_Set.Set);

private
   type Return_Values_Collection is record
      Get_Id : Infra_Id_Map.Map;
      Tread : Boolean;
      Tread_Existence : Boolean := FALSE;
      Is_Waiting_To_Enter_Stretch : Boolean;
      Is_Waiting_To_Enter_Stretch_Existence : Boolean := FALSE;
      Leave : Boolean;
      Leave_Existence : Boolean := FALSE;
      Is_Before : Infra_Id_Multimap.Map;
      Is_Before_Existence : Infra_Id_Multimap.Map;
      Get_Host : Infra_Id_Map.Map;
      Has_Host : Infra_Id_To_Boolean_Map.Map;
      Find_Intersections : Infra_Id_Multimap.Map;
   end record;

   type Object is new Stretch.Utils.Object with record
      Return_Values : Return_Values_Collection;
   end record;

end Reactive.Infrastructure.Stretch.Utils.Mock;
