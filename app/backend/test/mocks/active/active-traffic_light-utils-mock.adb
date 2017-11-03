with Mock.Exceptions;
use Mock.Exceptions;

package body Active.Traffic_Light.Utils.Mock is

   function Create return Traffic_Light.Utils.Mock.Reference
   is (new Traffic_Light.Utils.Mock.Object);

   function Is_A_Traffic_Light (This       : in out Mock.Object;
                                Active_Id : in     Agent.Agent_Id)
   return Boolean is
   begin
      if not This.Return_Values.Is_A_Traffic_Light_Existence or
         not This.Return_Values.Is_A_Traffic_Light.Contains (Active_Id)
      then
         Raise_Missing_Return_Value_For_Mocked_Procedure_Exception (
            Parameter_Name => "Is_A_Traffic_Light",
            Procedure_Name => "Is_A_Traffic_Light",
            Package_Name   => "Active.Traffic_Light.Utils.Mock");
      end if;

      return This.Return_Values.Is_A_Traffic_Light.Element (Active_Id);
   end Is_A_Traffic_Light;

   procedure Set_Value_For_Is_A_Traffic_Light (
      This               : in out Mock.Object;
      Active_Id          : in     Agent.Agent_Id;
      Is_A_Traffic_Light : in     Boolean) is
   begin
      This.Return_Values.Is_A_Traffic_Light.Insert (
         Active_Id, Is_A_Traffic_Light);
      This.Return_Values.Is_A_Traffic_Light_Existence := True;
   end Set_Value_For_Is_A_Traffic_Light;

end Active.Traffic_Light.Utils.Mock;
