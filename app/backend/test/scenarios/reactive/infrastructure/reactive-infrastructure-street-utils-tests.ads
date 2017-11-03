with AUnit;
with AUnit.Test_Cases;

package Reactive.Infrastructure.Street.Utils.Tests is
   package AU renames AUnit;
   package TC renames AUnit.Test_Cases;

   type Street_Utils_Test is new TC.Test_Case with null record;

   overriding procedure Set_Up (T : in out Street_Utils_Test);

   -- Test Routines:
   procedure Test_Id_Getter (T: in out TC.Test_Case'Class);
   procedure Test_Is_Contained_By (T: in out TC.Test_Case'Class);
   procedure Test_Is_Not_Contained_By (T: in out TC.Test_Case'Class);
   procedure Test_Orientation_Getter (T: in out TC.Test_Case'Class);
   procedure Test_Is_Treadable_In_Direction (T: in out TC.Test_Case'Class);
   procedure Test_Is_Not_Treadable_In_Direction (T: in out TC.Test_Case'Class);
   procedure Test_Lanes_By_Direction_Finder (T: in out TC.Test_Case'Class);

   procedure Register_Tests (T : in out Street_Utils_Test);
   overriding function Name (T : in Street_Utils_Test) return AU.Message_String;

end Reactive.Infrastructure.Street.Utils.Tests;
