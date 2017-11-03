with AUnit;
with AUnit.Test_Cases;

with Active.Traveller.Vehicle.Tests;

package Active.Traveller.Vehicle.Private_Motor_Vehicle.Tests is
   package AU renames AUnit;
   package TC renames AUnit.Test_Cases;

   type Private_Motor_Vehicle_Test is new Vehicle.Tests.Vehicle_Test
   with null record;

   overriding procedure Set_Up (T: in out Private_Motor_Vehicle_Test);

   procedure Register_Tests (T: in out Private_Motor_Vehicle_Test);
   overriding function Name (T: in Private_Motor_Vehicle_Test) return AU.Message_String;

end Active.Traveller.Vehicle.Private_Motor_Vehicle.Tests;
