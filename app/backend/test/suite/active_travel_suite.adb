with Active.Travel.Tests;
with Active.Travel.Travel_Planning.Tests;
with Active.Travel.Travel_Progress.Tests;
with Active.Travel.Travel_Completed.Tests;

package body Active_Travel_Suite is
   Result  : aliased TS.Test_Suite;
   Travel_Test
   : aliased Active.Travel.Tests.Travel_Test;
   Travel_Planning_Test
   : aliased Active.Travel.Travel_Planning.Tests.Travel_Planning_Test;
   Travel_Progress_Test
   : aliased Active.Travel.Travel_Progress.Tests.Travel_Progress_Test;
   Travel_Completed_Test
   : aliased Active.Travel.Travel_Completed.Tests.Travel_Completed_Test;

   function Suite
      return TS.Access_Test_Suite is
   begin
      TS.Add_Test (Result'Access, Travel_Test'Access);
      TS.Add_Test (Result'Access, Travel_Planning_Test'Access);
      TS.Add_Test (Result'Access, Travel_Progress_Test'Access);
      TS.Add_Test (Result'Access, Travel_Completed_Test'Access);
      return Result'Access;
   end Suite;
end Active_Travel_Suite;
