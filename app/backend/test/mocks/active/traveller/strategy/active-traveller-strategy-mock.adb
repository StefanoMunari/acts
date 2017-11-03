with Mock.Exceptions;

use Mock.Exceptions;

package body Active.Traveller.Strategy.Mock is

   function Create
   return Strategy.Mock.Reference
   is (new Strategy.Mock.Object);

   overriding
   function Wait_For_Bus_Or_Not (
      This           : Strategy.Mock.Object;
      Pedestrian_Id  : Agent.Agent_Id;
      Current_Stretch : Infra_Id;
      Bus_Stop_Ref   : Road_Sign.Bus_Stop.Reference)
   return Boolean is
   begin
      if not This.Return_Values.Wait_For_Bus_Or_Not_Existence then
         Raise_Missing_Return_Value_For_Mocked_Procedure_Exception (
            Procedure_Name => "Wait_For_Bus_Or_Not",
            Package_Name   => "Active.Traveller.Strategy.Mock");
      end if;

      return This.Return_Values.Wait_For_Bus_Or_Not;
   end Wait_For_Bus_Or_Not;

   not overriding
   procedure Set_Return_Value_For_Wait_For_Bus_Or_Not (
      This         : in out Strategy.Mock.Object;
      Return_Value : in     Boolean) is
   begin
      This.Return_Values.Wait_For_Bus_Or_Not := Return_Value;
      This.Return_Values.Wait_For_Bus_Or_Not_Existence := True;
   end Set_Return_Value_For_Wait_For_Bus_Or_Not;

end Active.Traveller.Strategy.Mock;
