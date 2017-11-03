with Reactive.District.Mock;
with Mock.Exceptions;

use Mock.Exceptions;

package body Active.Bus_Service.Utils.Mock is

   use Agent;

   function Create return Bus_Service.Utils.Mock.Reference
   is (new Bus_Service.Utils.Mock.Object);

   function Is_A_Bus_Service (
      This         : in out Mock.Object;
      Traveller_Id : in     Agent.Agent_Id)
   return Boolean is
   begin
      if not This.Return_Values.Traveller_Id_Existence then
         Raise_Missing_Return_Value_For_Mocked_Procedure_Exception
           (Procedure_Name => "Is_A_Bus_Service",
            Package_Name   => "Active.Bus_Service.Utils.Mock");
      end if;
      return TRUE;
   end;

   procedure On_Bus_Stop (This         : in out Mock.Object;
                          Traveller_Id : in     Agent.Agent_Id) is
   begin
     if not This.Return_Values.Traveller_Id_Existence then
         Raise_Missing_Return_Value_For_Mocked_Procedure_Exception
           (Procedure_Name => "Is_A_Bus_Service",
            Package_Name   => "Active.Bus_Service.Utils.Mock");
      end if;

      if This.Return_Values.Traveller_Id = Traveller_Id then
         This.Return_Values.Bus_Service_Ref.On_Bus_Stop;
      end if;
   end;

   procedure Set_Traveller_For_On_Bus_Stop (
      This         : in out Mock.Object;
      Traveller_Id : in     Agent.Agent_Id;
      Bus_Service_Ref : in     Bus_Service.Mock.Reference) is
     begin
       This.Return_Values.Traveller_Id := Traveller_Id;
       This.Return_Values.Traveller_id_Existence := TRUE;
       This.Return_Values.Bus_Service_Ref := Bus_Service_Ref;
       This.Return_Values.Bus_Service_Ref_Existence := TRUE;
     end;

end Active.Bus_Service.Utils.Mock;
