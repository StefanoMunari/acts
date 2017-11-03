with AUnit;
with AUnit.Test_Cases;
with Reactive.Infrastructure.Stretch.Decoration.Zebra_Crossing.Tests;

package Reactive.Infrastructure.Stretch.Decoration.Bicycle_Crossing.Tests is
   package AU renames AUnit;
   package TC renames AUnit.Test_Cases;

   type Bicycle_Crossing_Test is new Zebra_Crossing.Tests.Zebra_Crossing_Test
      with null record;

   overriding
   procedure Set_Up (T : in out Bicycle_Crossing_Test);

   -- Test Routines:
   procedure Test_Bicycle_Has_Priority (T : in out Bicycle_Crossing_Test);
   procedure Test_Pedestrian_Does_Not_Have_Priority
      (T : in out Bicycle_Crossing_Test);
   procedure Test_Bus_Does_Not_Have_Priority
      (T : in out Bicycle_Crossing_Test);
   procedure Test_PMV_Does_Not_Have_Priority
      (T : in out Bicycle_Crossing_Test);

   procedure Register_Tests (T : in out Bicycle_Crossing_Test);
   overriding
   function Name (T : in Bicycle_Crossing_Test)
      return AU.Message_String;

end Reactive.Infrastructure.Stretch.Decoration.Bicycle_Crossing.Tests;
