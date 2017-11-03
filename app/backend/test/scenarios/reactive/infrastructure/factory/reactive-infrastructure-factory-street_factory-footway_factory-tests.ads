with AUnit;
with AUnit.Test_Cases;

package Reactive.Infrastructure.Factory.Street_Factory.Footway_Factory.Tests is
   package AU renames AUnit;
   package TC renames AUnit.Test_Cases;

   type Footway_Factory_Test is new TC.Test_Case with null record;

   overriding procedure Set_Up (T : in out Footway_Factory_Test);

   -- Test Routines:
   procedure Test_Create_Footway_Stretch (T: in out TC.Test_Case'Class);
   procedure Test_Create_Footway_Lane (T: in out TC.Test_Case'Class);

   procedure Register_Tests (T : in out Footway_Factory_Test);
   overriding function Name (T : in Footway_Factory_Test)
      return AU.Message_String;

end Reactive.Infrastructure.Factory.Street_Factory.Footway_Factory.Tests;
