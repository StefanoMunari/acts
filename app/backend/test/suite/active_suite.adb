with Active.People_Carrier.Utils.Tests;

package body Active_Suite is
   Result  : aliased TS.Test_Suite;
   People_Carrier_Utils_Test : aliased Active.People_Carrier.Utils.Tests.People_Carrier_Utils_Test;

   function Suite
      return TS.Access_Test_Suite is
   begin
      TS.Add_Test (Result'Access, People_Carrier_Utils_Test'Access);
      return Result'Access;
   end Suite;
end Active_Suite;
