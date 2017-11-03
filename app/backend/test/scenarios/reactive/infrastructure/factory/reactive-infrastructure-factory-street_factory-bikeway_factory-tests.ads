with AUnit;
with AUnit.Test_Cases;

package Reactive.Infrastructure.Factory.Street_Factory.Bikeway_Factory.Tests is
   package AU renames AUnit;
   package TC renames AUnit.Test_Cases;

   type Bikeway_Factory_Test is new TC.Test_Case with null record;

   overriding procedure Set_Up (T : in out Bikeway_Factory_Test);

   -- Test Routines:
   procedure Test_Create_Bikeway_Stretch (T: in out TC.Test_Case'Class);
   procedure Test_Create_Bikeway_Lane (T: in out TC.Test_Case'Class);

   procedure Register_Tests (T : in out Bikeway_Factory_Test);
   overriding function Name (T : in Bikeway_Factory_Test)
      return AU.Message_String;

end Reactive.Infrastructure.Factory.Street_Factory.Bikeway_Factory.Tests;
