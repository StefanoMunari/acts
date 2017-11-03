with AUnit;
with AUnit.Test_Cases;
with Reactive.Infrastructure.Stretch.Tests;

package Reactive.Infrastructure.Stretch.Bikeway_Stretch.Tests is
   package AU renames AUnit;
   package TC renames AUnit.Test_Cases;

   type Bikeway_Stretch_Test is new Stretch.Tests.Stretch_Test with null record;

   overriding procedure Set_Up (T: in out Bikeway_Stretch_Test);

   procedure Register_Tests (T: in out Bikeway_Stretch_Test);
   overriding function Name (T: in Bikeway_Stretch_Test) return AU.Message_String;

end Reactive.Infrastructure.Stretch.Bikeway_Stretch.Tests;
