with Mock.Exceptions;

use Mock.Exceptions;

package body Active.Traffic_Light.Mock is

   function New_Mock return Traffic_Light.Mock.Reference
   is (new Traffic_Light.Mock.Object);

   function Get_Id (This : in Traffic_Light.Mock.Object)
   return Agent.Agent_Id is
   begin
      if not This.Mock_Values.Id_Existence then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception
           (Function_Name  => "Get_Id",
            Package_Name   => "Active.Traffic_Light.Mock");
      end if;

      return This.Mock_Values.Id;
   end Get_Id;

   procedure Set_Id (
      This  : in out Traffic_Light.Mock.Object;
      Id    : in Agent.Agent_Id) is
   begin
      This.Mock_Values.Id := Id;
      This.Mock_Values.Id_Existence := TRUE;
   end Set_Id;

end Active.Traffic_Light.Mock;
