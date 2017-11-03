with AUnit;
with AUnit.Test_Cases;

package Reactive.Infrastructure.Utils.Tests is
   package AU renames AUnit;
   package TC renames AUnit.Test_Cases;

   type Infrastructure_Utils_Test is new TC.Test_Case with null record;

   overriding procedure Set_Up (T : in out Infrastructure_Utils_Test);

   -- Test Routines:
   procedure Test_Exists (T: in out TC.Test_Case'Class);
   procedure Test_Not_Exists (T: in out TC.Test_Case'Class);

   procedure Register_Tests (T : in out Infrastructure_Utils_Test);
   overriding function Name (T : in Infrastructure_Utils_Test)
                             return AU.Message_String;

end Reactive.Infrastructure.Utils.Tests;

