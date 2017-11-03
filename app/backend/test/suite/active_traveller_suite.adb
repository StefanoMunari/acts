with Active.Traveller.Pedestrian.Tests;
with Active.Traveller.Strategy.Simple.Tests;
with Active.Traveller.Vehicle.Bicycle.Tests;
with Active.Traveller.Vehicle.Bus.Tests;
with Active.Traveller.Vehicle.Private_Motor_Vehicle.Tests;
with Active.Traveller.Utils.Tests;

package body Active_Traveller_Suite is
   Result  : aliased TS.Test_Suite;
   Pedestrian_Test : aliased Active.Traveller.Pedestrian.Tests.Pedestrian_Test;
   Simple_Test : aliased Active.Traveller.Strategy.Simple.Tests.Simple_Test;
   Bicycle_Test : aliased Active.Traveller.Vehicle.Bicycle.Tests.Bicycle_Test;
   Bus_Test : aliased Active.Traveller.Vehicle.Bus.Tests.Bus_Test;
   Private_Motor_Vehicle_Test :
      aliased Active.Traveller.Vehicle.Private_Motor_Vehicle.Tests.Private_Motor_Vehicle_Test;
   Traveller_Utils_Test :
      aliased Active.Traveller.Utils.Tests.Traveller_Utils_Test;

   function Suite
      return TS.Access_Test_Suite is
   begin
      TS.Add_Test (Result'Access, Pedestrian_Test'Access);
      TS.Add_Test (Result'Access, Simple_Test'Access);
      TS.Add_Test (Result'Access, Bicycle_Test'Access);
      TS.Add_Test (Result'Access, Bus_Test'Access);
      TS.Add_Test (Result'Access, Private_Motor_Vehicle_Test'Access);
      TS.Add_Test (Result'Access, Traveller_Utils_Test'Access);
      return Result'Access;
   end Suite;
end Active_Traveller_Suite;
