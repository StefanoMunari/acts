with Mock.Exceptions;

use Mock.Exceptions;

package body Reactive.Infrastructure.Building.Host.Use_Carrier_Strategy.Mock is

   function Create return Use_Carrier_Strategy.Reference
   is (new Mock.Object);

   overriding
   function Use_Carrier_Or_Not (This : in out Mock.Object)
   return Boolean is
   begin
      if not This.Return_Values.Use_Carrier_Or_Not_Existence then
         Raise_Missing_Return_Value_For_Mocked_Procedure_Exception
           (Parameter_Name => "Returned value",
            Procedure_Name => "Use_Carrier_Or_Not",
            Package_Name
            => "Reactive.Infrastructure.Stretch.Building.Use_Carrier_Strategy.Mock");
      end if;

     return This.Return_Values.Use_Carrier_Or_Not;
   end Use_Carrier_Or_Not;

   procedure Set_Return_Value_For_Use_Carrier_Or_Not
     (This         : in out Mock.Object;
      Return_Value : in     Boolean) is
   begin
      This.Return_Values.Use_Carrier_Or_Not           := Return_Value;
      This.Return_Values.Use_Carrier_Or_Not_Existence := True;
   end Set_Return_Value_For_Use_Carrier_Or_Not;

end Reactive.Infrastructure.Building.Host.Use_Carrier_Strategy.Mock;
