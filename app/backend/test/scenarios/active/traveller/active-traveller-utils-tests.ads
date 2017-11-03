with AUnit;
with AUnit.Test_Cases;

package Active.Traveller.Utils.Tests is
   package AU renames AUnit;
   package TC renames AUnit.Test_Cases;

   type Traveller_Utils_Test is new TC.Test_Case with null record;

   overriding procedure Set_Up (T : in out Traveller_Utils_Test);

   -- Test Routines:
   -- Add some?

   procedure Register_Tests (T : in out Traveller_Utils_Test);
   overriding function Name (T : in Traveller_Utils_Test) return AU.Message_String;

end Active.Traveller.Utils.Tests;

