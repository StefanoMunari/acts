with AUnit;
with AUnit.Test_Cases;

package Reactive.Infrastructure.Way.Utils.Tests is
   package AU renames AUnit;
   package TC renames AUnit.Test_Cases;

   type Way_Utils_Test is new TC.Test_Case with null record;

   overriding procedure Set_Up (T : in out Way_Utils_Test);

   -- Test Routines:
   procedure Test_Find_Street (T: in out TC.Test_Case'Class);
   procedure Test_Is_Contained_By (T: in out TC.Test_Case'Class);
   procedure Test_Is_Not_Contained_By (T: in out TC.Test_Case'Class);

   procedure Register_Tests (T : in out Way_Utils_Test);
   overriding function Name (T : in Way_Utils_Test) return AU.Message_String;

end Reactive.Infrastructure.Way.Utils.Tests;

