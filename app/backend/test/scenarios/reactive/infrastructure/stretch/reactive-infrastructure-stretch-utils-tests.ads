with AUnit;
with AUnit.Test_Cases;

package Reactive.Infrastructure.Stretch.Utils.Tests is
   package AU renames AUnit;
   package TC renames AUnit.Test_Cases;

   type Stretch_Utils_Test is new TC.Test_Case with null record;

   overriding procedure Set_Up (T : in out Stretch_Utils_Test);

   -- Test Routines:
   procedure Test_Id_Getter (T: in out TC.Test_Case'Class);
   procedure Test_Tread (T: in out TC.Test_Case'Class);
   procedure Test_Not_Tread (T: in out TC.Test_Case'Class);
   procedure Test_Traveller_Is_Waiting_To_Enter_Stretch
     (T: in out TC.Test_Case'Class);
   procedure Test_Is_Before (T: in out TC.Test_Case'Class);
   procedure Test_Leave (T: in out TC.Test_Case'Class);
   procedure Test_Not_Leave (T: in out TC.Test_Case'Class);

   procedure Register_Tests (T : in out Stretch_Utils_Test);
   overriding function Name (T : in Stretch_Utils_Test) return AU.Message_String;

end Reactive.Infrastructure.Stretch.Utils.Tests;

