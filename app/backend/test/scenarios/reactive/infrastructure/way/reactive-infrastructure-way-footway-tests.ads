with AUnit;
with AUnit.Test_Cases;
with Reactive.Infrastructure.Way.Tests;

package Reactive.Infrastructure.Way.Footway.Tests is
   package AU renames AUnit;
   package TC renames AUnit.Test_Cases;

   type Footway_Test is new Way.Tests.Way_Test with null record;

   overriding procedure Set_Up (T: in out Footway_Test);

   procedure Register_Tests (T: in out Footway_Test);
   overriding function Name (T: in Footway_Test) return AU.Message_String;

end Reactive.Infrastructure.Way.Footway.Tests;
