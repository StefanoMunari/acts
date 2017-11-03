with AUnit;
with AUnit.Test_Cases;

package Reactive.Infrastructure.Way.Roadway.Utils.Tests is
   package AU renames AUnit;
   package TC renames AUnit.Test_Cases;

   type Roadway_Utils_Test is new TC.Test_Case with null record;

   overriding procedure Set_Up (T : in out Roadway_Utils_Test);

   -- Test Routines:
   procedure Test_Lane_By_Direction_Finder (T : in out TC.Test_Case'Class);

   procedure Register_Tests (T : in out Roadway_Utils_Test);
   overriding function Name (T : in Roadway_Utils_Test) return AU.Message_String;

end Reactive.Infrastructure.Way.Roadway.Utils.Tests;

