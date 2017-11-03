with Active.Agent;

package Reactive.Infrastructure.Stretch.Mock is

   package Agent renames Active.Agent;

   type Object is new Stretch.Object with private;
   type Reference is access all Stretch.Mock.Object'Class;

   function Create return Stretch.Mock.Reference;

   overriding
   procedure Tread (This         : in out Stretch.Mock.Object;
                    Traveller_Id : in     Agent.Agent_Id;
                    Advanced     :    out Boolean);

   overriding
   function Find_Street (This : in Stretch.Mock.Object)
                         return Infra_Id;

   overriding
   function Get_Id (This : in Stretch.Mock.Object) return Infra_Id;

   overriding
   function Find_Lane (This : in Stretch.Mock.Object)
                       return Infra_Id;

   overriding
   function Calculate_Position (This : in Stretch.Mock.Object)
                                return Natural;

   overriding
   function Is_Waiting_To_Enter_Stretch (
      This : in Stretch.Mock.Object;
      Traveller_Id : in Agent.Agent_Id) return Boolean;

   overriding
   procedure Leave (
      This         : in out Stretch.Mock.Object;
      Traveller_Id : in Agent.Agent_Id;
      Leaved       : out Boolean);

   overriding
   function Is_Before (This, Other: in Stretch.Mock.Object)
                       return Boolean;

   overriding
   procedure Set_Lane (
      This    : in out Stretch.Mock.Object;
      Lane_Id : in     Infra_Id);

   overriding
   procedure Set_Host (
      This    : in out Stretch.Mock.Object;
      Host_Id : in     Infra_Id);

   overriding
   function Get_Host (This : in Stretch.Mock.Object)
   return Infra_Id;

   overriding
   function Has_Host (This : in Stretch.Mock.Object)
   return Boolean;

   overriding
   function Find_Intersections (This : in Stretch.Mock.Object)
                                return Infra_Id_Set.Set;

   overriding
   function Is_Contained_By (This         : in Stretch.Mock.Object;
                             Container_Id : in Infra_Id) return Boolean;

   not overriding
   procedure Set_Return_Value_For_Tread (
      This     : in out Stretch.Mock.Object;
      Advanced : in     Boolean);

   not overriding
   procedure Set_Return_Value_For_Find_Street (
      This         : in out Stretch.Mock.Object;
      Return_Value : in     Infra_Id);

   not overriding
   procedure Set_Id (
      This         : in out Stretch.Mock.Object;
      Return_Value : in     Infra_Id);

   not overriding
   procedure Set_Return_Value_For_Find_Lane (
      This         : in out Stretch.Mock.Object;
      Return_Value : in     Infra_Id);

   not overriding
   procedure Set_Return_Value_For_Calculate_Position (
      This         : in out Stretch.Mock.Object;
      Return_Value : in     Natural);

   not overriding
   procedure Set_Return_Value_For_Is_Waiting_To_Enter_Stretch (
      This         : in out Stretch.Mock.Object;
      Return_Value : in     Boolean);

   not overriding
   procedure Set_Return_Value_For_Leave (
      This   : in out Stretch.Mock.Object;
      Leaved : in     Boolean);

   not overriding
   procedure Set_Return_Value_For_Is_Before (
      This         : in out Stretch.Mock.Object;
      Return_Value : in     Boolean);

   not overriding
   procedure Set_Return_Value_For_Find_Intersections (
      This         : in out Stretch.Mock.Object;
      Return_Value : in     Infra_Id_Set.Set);

   not overriding
   procedure Set_Return_Value_For_Is_Contained_By (
      This         : in out Stretch.Mock.Object;
      Return_Value : in     Boolean);

   not overriding
   procedure Set_Return_Value_For_Get_Host (
      This         : in out Stretch.Mock.Object;
      Return_Value : in     Infra_Id);

   not overriding
   procedure Set_Return_Value_For_Has_Host (
      This         : in out Stretch.Mock.Object;
      Return_Value : in     Boolean);

   not overriding
   function Get_Value_For_Set_Host_Id (This : in out Stretch.Mock.Object)
   return Infra_Id;

   not overriding
   procedure Set_Return_Value_For_Get_Travellers_Queue (
      This         : in out Stretch.Mock.Object;
      Return_Value : access Stretch.Protected_Travellers_Queue);

private
   type Mock_Values_Collection is record
      Id        : Infra_Id;
      Id_Existence : Boolean := FALSE;
      Lane_Id   : Infra_Id;
      Lane_Id_Existence : Boolean := FALSE;
      Set_Host_Id : Infra_Id;
      Set_Host_Id_Called : Boolean := FALSE;
   end record;

   type Return_Values_Collection is record
      Tread : Boolean;
      Tread_Existence : Boolean := FALSE;
      Find_Street : Infra_Id;
      Find_Street_Existence : Boolean := FALSE;
      Find_Lane : Infra_Id;
      Find_Lane_Existence : Boolean := FALSE;
      Calculate_Position : Natural;
      Calculate_Position_Existence : Boolean := FALSE;
      Is_Waiting_To_Enter_Stretch : Boolean;
      Is_Waiting_To_Enter_Stretch_Existence : Boolean := FALSE;
      Leave : Boolean;
      Leave_Existence : Boolean := FALSE;
      Is_Before : Boolean;
      Is_Before_Existence : Boolean := FALSE;
      Find_Intersections : Infra_Id_Set.Set;
      Find_Intersections_Existence : Boolean := FALSE;
      Is_Contained_By : Boolean;
      Is_Contained_By_Existence : Boolean := FALSE;
      Get_Host   : Infra_Id;
      Get_Host_Existence : Boolean := FALSE;
      Has_Host   : Boolean;
      Has_Host_Existence : Boolean := FALSE;
      Get_Travellers_Queue : access Stretch.Protected_Travellers_Queue;
      Get_Travellers_Queue_Existence : Boolean := FALSE;
   end record;

   type Object is
     new Stretch.Object with record
      Mock_Values  : Mock_Values_Collection;
      Return_Values : Return_Values_Collection;
   end record;

   overriding
   procedure Initialize (This : in out Stretch.Mock.Object) is Null;

   overriding
   procedure Finalize (This : in out Stretch.Mock.Object) is Null;

   overriding
   function Get_Travellers_Queue (This : in Stretch.Mock.Object)
   return access Protected_Travellers_Queue;

end Reactive.Infrastructure.Stretch.Mock;
