package Reactive.Treadable.Mock is

   use Reactive.Infra_Id_Type;

   type Object is new Treadable.Object with private;
   type Reference is access all Treadable.Mock.Object'Class;

   not overriding
   function Create return Treadable.Mock.Reference;

   overriding
   procedure Tread (
      This         : in out Treadable.Mock.Object;
      Traveller_Id : in     Agent.Agent_Id;
      Advanced     :    out Boolean);

   overriding
   procedure Leave (
      This         : in out Treadable.Mock.Object;
      Traveller_Id : in     Agent.Agent_Id;
      Left         :    out Boolean);

   overriding
   function Get_Id (This : in Treadable.Mock.Object)
   return Infra_Id;

   not overriding
   procedure Set_Return_Value_For_Tread (
      This         : in out Treadable.Mock.Object;
      Return_Value : in     Boolean);

   not overriding
   procedure Set_Return_Value_For_Leave (
      This         : in out Treadable.Mock.Object;
      Return_Value : in     Boolean);

   not overriding
   procedure Set_Return_Value_For_Get_Id (
      This         : in out Treadable.Mock.Object;
      Return_Value : in     Infra_Id);

private
   type Return_Values_Collection is record
      Tread : Boolean;
      Tread_Existence : Boolean := FALSE;
      Leave : Boolean;
      Leave_Existence : Boolean := FALSE;
      Get_Id : Infra_Id;
      Get_Id_Existence : Boolean := FALSE;
   end record;

   type Object is new Treadable.Object with record
      Return_Values : Return_Values_Collection;
   end record;

end Reactive.Treadable.Mock;
