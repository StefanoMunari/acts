with AUnit;
with AUnit.Test_Cases;

package Reactive.Infrastructure.Intersection.Utils.Tests is
   package AU renames AUnit;
   package TC renames AUnit.Test_Cases;

   type Intersection_Utils_Test is new TC.Test_Case with null record;

   overriding procedure Set_Up (T : in out Intersection_Utils_Test);

   -- Test Routines:
   procedure Test_Street_Direction_Finder (T: in out TC.Test_Case'Class);
   procedure Test_Streets_Connected_With_Intersection_Finder
     (T: in out TC.Test_Case'Class);

   procedure Register_Tests (T : in out Intersection_Utils_Test);
   overriding function Name (T : in Intersection_Utils_Test) return AU.Message_String;

end Reactive.Infrastructure.Intersection.Utils.Tests;

