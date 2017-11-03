with Mock.Exceptions;

use Mock.Exceptions;

package body Reactive.Treadable.Mock is

   function Create return Treadable.Mock.Reference
   is (new Treadable.Mock.Object);

   procedure Tread (
      This         : in out Treadable.Mock.Object;
      Traveller_Id : in     Agent.Agent_Id;
      Advanced     :    out Boolean) is
   begin
      if not This.Return_Values.Tread_Existence then
         Raise_Missing_Return_Value_For_Mocked_Procedure_Exception (
            Parameter_Name => "Advanced",
            Procedure_Name => "Tread",
            Package_Name   => "Treadable.Mock");
      end if;

      Advanced := This.Return_Values.Tread;
   end Tread;

   procedure Leave (
      This         : in out Treadable.Mock.Object;
      Traveller_Id : in     Agent.Agent_Id;
      Left         :    out Boolean) is
   begin
      if not This.Return_Values.Leave_Existence then
         Raise_Missing_Return_Value_For_Mocked_Procedure_Exception (
            Parameter_Name => "Left",
            Procedure_Name => "Leave",
            Package_Name   => "Treadable.Mock");
      end if;

      Left := This.Return_Values.Leave;
   end Leave;

   function Get_Id (This : in Treadable.Mock.Object)
   return Infra_Id
   is
   begin
      if not This.Return_Values.Get_Id_Existence then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception (
            Function_Name => "Get_Id",
            Package_Name  => "Treadable.Mock");
      end if;

      return This.Return_Values.Get_Id;
   end Get_Id;

   procedure Set_Return_Value_For_Tread (
      This         : in out Treadable.Mock.Object;
      Return_Value : in     Boolean)
   is
   begin
      This.Return_Values.Tread := Return_Value;
      This.Return_Values.Tread_Existence := True;
   end Set_Return_Value_For_Tread;

   procedure Set_Return_Value_For_Leave (
      This         : in out Treadable.Mock.Object;
      Return_Value : in     Boolean)
   is
   begin
      This.Return_Values.Leave := Return_Value;
      This.Return_Values.Leave_Existence := True;
   end Set_Return_Value_For_Leave;

   procedure Set_Return_Value_For_Get_Id (
      This         : in out Treadable.Mock.Object;
      Return_Value : in     Infra_Id)
   is
   begin
      This.Return_Values.Get_Id := Return_Value;
      This.Return_Values.Get_Id_Existence := True;
   end Set_Return_Value_For_Get_Id;

end Reactive.Treadable.Mock;
