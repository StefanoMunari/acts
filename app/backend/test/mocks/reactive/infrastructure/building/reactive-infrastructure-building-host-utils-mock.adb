with Mock.Exceptions;

use Mock.Exceptions;

package body Reactive.Infrastructure.Building.Host.Utils.Mock is

   function Create return Host.Utils.Mock.Reference
   is (new Host.Utils.Mock.Object);

   function Stop_Over (This         : in out Host.Utils.Mock.Object;
                       Host_Id      : in     Infra_Id;
                       Traveller_Id : in     Agent.Agent_Id)
      return Agent_Id_List.List is
   begin
      if not This.Return_Values.Stop_Over_Existence then
         Raise_Missing_Return_Value_For_Mocked_Procedure_Exception (
            Parameter_Name => "Returned value",
            Procedure_Name => "Stop_Over",
            Package_Name   =>
               "Reactive.Infrastructure.Building.Host.Utils.Mock");
      end if;

      return This.Return_Values.Stop_Over;
   end Stop_Over;

   function Dump (
      This    : Host.Utils.Mock.Object;
      Host_Id : Infra_Id)
   return G_JSON.JSON_Value is
   begin
      if not This.Return_Values.Dump_Existence then
         Raise_Missing_Return_Value_For_Mocked_Procedure_Exception (
            Parameter_Name => "Returned value",
            Procedure_Name => "Dump",
            Package_Name   =>
               "Reactive.Infrastructure.Building.Host.Utils.Mock");
      end if;

      return This.Return_Values.Dump;
   end Dump;

   procedure Set_Travellers_List_For_Stop_Over (
      This   : in out Host.Utils.Mock.Object;
      Agents : in     Agent_Id_List.List) is
   begin
      This.Return_Values.Stop_Over := Agents;
      This.Return_Values.Stop_Over_Existence := True;
   end Set_Travellers_List_For_Stop_Over;

   not overriding
   procedure Set_Return_Value_For_Dump (
      This         : in out Host.Utils.Mock.Object;
      Return_Value : in     G_JSON.JSON_Value) is
   begin
      This.Return_Values.Dump := Return_Value;
      This.Return_Values.Dump_Existence := True;
   end Set_Return_Value_For_Dump;

end Reactive.Infrastructure.Building.Host.Utils.Mock;
