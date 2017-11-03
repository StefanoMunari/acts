with Mock.Exceptions; use Mock.Exceptions;

package body Active.Agent.Mock is

   function Create return Agent.Reference
   is (new Agent.Mock.Object);

   function Get_Id (This : Agent.Mock.Object) return Agent_Id is
   begin
      if not This.Return_Values.Id_Existence
      then
         Raise_Missing_Return_Value_For_Mocked_Procedure_Exception (
            Parameter_Name => "Id",
            Procedure_Name => "Get_Id",
            Package_Name   => "Active.Agent.Mock");
      end if;

      return This.Return_Values.Id;
   end Get_Id;

   procedure Act (This : in out Active.Agent.Mock.Object) is
   begin
      This.Rendezvous.Notify;
   end Act;

   procedure Set_Id (This : in out Agent.Mock.Object; Id : in Agent_Id) is
   begin
      This.Return_Values.Id := Id;
      This.Return_Values.Id_Existence := True;
   end Set_Id;

   function Get_Act_Called (This : Agent.Mock.Object) return Boolean
   is
   begin
      This.Rendezvous.Wait;
      return True;
   end Get_Act_Called;

   protected body Rendezvous_Object is

      entry Wait
         when Act_Called is
      begin
         Act_Called := False;
      end Wait;

      entry Notify
         when True is
      begin
         Act_Called := True;
      end Notify;

   end Rendezvous_Object;

end Active.Agent.Mock;
