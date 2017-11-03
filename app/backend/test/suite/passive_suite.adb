with Passive.Road_Sign.Speed_Limit.Tests;
with Passive.Road_Sign.Bus_Stop.Tests;
with Ada.Text_IO; use Ada.Text_IO;


package body Passive_Suite is
   Result  : aliased TS.Test_Suite;
   Speed_Limit_Suite : aliased Passive.Road_Sign.Speed_Limit.Tests.Speed_Limit_Test;
   Bus_Stop_Suite : aliased Passive.Road_Sign.Bus_Stop.Tests.Bus_Stop_Test;

   function Suite
      return TS.Access_Test_Suite is
   begin
      TS.Add_Test (Result'Access, Speed_Limit_Suite'Access);
      TS.Add_Test (Result'Access, Bus_Stop_Suite'Access);
   return Result'Access;
   end Suite;
end Passive_Suite;
