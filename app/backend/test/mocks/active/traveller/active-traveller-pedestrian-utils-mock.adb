with Mock.Exceptions;

use Mock.Exceptions;

package body Active.Traveller.Pedestrian.Utils.Mock is

   function Create
   return Pedestrian.Utils.Mock.Reference
   is (new Pedestrian.Utils.Mock.Object);

   function Is_A_Pedestrian (
      This         : in Pedestrian.Utils.Mock.Object;
      Traveller_Id : in Agent.Agent_Id)
   return Boolean
   is
   begin
      if not This.Return_Values.Is_A_Pedestrian.Contains (Traveller_Id) then
         Raise_Missing_Return_Value_For_Mocked_Procedure_Exception (
            Procedure_Name => "Is_A_Pedestrian",
            Package_Name   => "Active.Traveller.Pedestrian.Utils.Mock");
      end if;

      return This.Return_Values.Is_A_Pedestrian.Element (Traveller_Id);
   end Is_A_Pedestrian;

   procedure Stop_Waiting (
      This         : in out Pedestrian.Utils.Mock.Object;
      Traveller_Id : in     Agent.Agent_Id)
   is
   begin
      This.Mock_Values.Stop_Waiting.Include (Traveller_Id);
   end Stop_Waiting;

   procedure Set_Return_Value_For_Is_A_Pedestrian (
      This         : in out Pedestrian.Utils.Mock.Object;
      Traveller_Id : in     Agent.Agent_Id;
      Return_Value : in     Boolean)
   is
   begin
      This.Return_Values.Is_A_Pedestrian.Insert (Traveller_Id, Return_Value);
   end Set_Return_Value_For_Is_A_Pedestrian;

   function Get_Stop_Waiting_For_Id (
      This         : in out Pedestrian.Utils.Mock.Object;
      Traveller_Id : in     Agent.Agent_Id)
   return Boolean
   is (This.Mock_Values.Stop_Waiting.Contains (Traveller_Id));

end Active.Traveller.Pedestrian.Utils.Mock;
