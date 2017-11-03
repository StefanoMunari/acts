with AUnit;
with AUnit.Test_Cases;

with Reactive.Infrastructure.Lane.Tests;

package Reactive.Infrastructure.Lane.Roadway_Lane.Tests is
   package AU renames AUnit;
   package TC renames AUnit.Test_Cases;

   type Roadway_Lane_Test is new Lane.Tests.Lane_Test with null record;

   overriding procedure Set_Up (T: in out Roadway_Lane_Test);

   procedure Register_Tests (T: in out Roadway_Lane_Test);
   overriding function Name (T: in Roadway_Lane_Test) return AU.Message_String;

end Reactive.Infrastructure.Lane.Roadway_Lane.Tests;
