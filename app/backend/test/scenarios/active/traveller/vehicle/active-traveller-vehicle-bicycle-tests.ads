with AUnit;
with AUnit.Test_Cases;

with Active.Traveller.Vehicle.Tests;

package Active.Traveller.Vehicle.Bicycle.Tests is
   package AU renames AUnit;
   package TC renames AUnit.Test_Cases;

   type Bicycle_Test is new Vehicle.Tests.Vehicle_Test with null record;

   overriding procedure Set_Up (T: in out Bicycle_Test);

   procedure Register_Tests (T: in out Bicycle_Test);
   overriding function Name (T: in Bicycle_Test) return AU.Message_String;

end Active.Traveller.Vehicle.Bicycle.Tests;
