with Mock.Exceptions;
use Mock.Exceptions;

with Reactive.District;

package body Reactive.Infrastructure.Utils.Mock is

   function Create return Infrastructure.Utils.Mock.Reference
   is (new Infrastructure.Utils.Mock.Object);

   function Exists (
      This              : in Infrastructure.Utils.Mock.Object;
      Infrastructure_Id : in Infra_Id) return Boolean is
   begin
      if not This.Return_Values.Exists_Existence then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception
            (Function_Name => "Exists",
             Package_Name  => "Infrastructure.Utils.Mock");
      end if;

      return This.Return_Values.Exists;
   end Exists;

   procedure Tread (
      This              : in     Infrastructure.Utils.Mock.Object;
      Old_Position      : in     Infra_Id;
      Infrastructure_Id : in     Infra_Id;
      Traveller_Id      : in     Agent.Agent_Id;
      Advanced          :    out Boolean) is
   begin
      if not This.Return_Values.Tread_Existence then
         Raise_Missing_Return_Value_For_Mocked_Procedure_Exception
           (Parameter_Name => "Advanced",
            Procedure_Name => "Tread",
            Package_Name   => "Infrastructure.Utils.Mock");
      end if;

      Advanced := This.Return_Values.Tread;
   end Tread;

   procedure Set_Return_Value_For_Exists (
      This         : in out Infrastructure.Utils.Mock.Object;
      Return_Value : in     Boolean) is
   begin
      This.Return_Values.Exists := Return_Value;
      This.Return_Values.Exists_Existence := TRUE;
   end Set_Return_Value_For_Exists;

   procedure Set_Return_Value_For_Tread (
      This         : in out Infrastructure.Utils.Mock.Object;
      Return_Value : in     Boolean) is
   begin
      This.Return_Values.Tread := Return_Value;
      This.Return_Values.Tread_Existence := TRUE;
   end Set_Return_Value_For_Tread;

end Reactive.Infrastructure.Utils.Mock;
