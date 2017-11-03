with AUnit;
with AUnit.Test_Cases;
with Reactive.Infrastructure.Way.Tests;

package Reactive.Infrastructure.Way.Bikeway.Tests is
   package AU renames AUnit;
   package TC renames AUnit.Test_Cases;

   type Bikeway_Test is new Way.Tests.Way_Test with null record;

   overriding procedure Set_Up (T: in out Bikeway_Test);

   procedure Register_Tests (T: in out Bikeway_Test);
   overriding function Name (T: in Bikeway_Test) return AU.Message_String;

end Reactive.Infrastructure.Way.Bikeway.Tests;
