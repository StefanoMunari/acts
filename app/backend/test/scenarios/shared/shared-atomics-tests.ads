with AUnit;
with AUnit.Test_Cases;

package Shared.Atomics.Tests is
   package AU renames AUnit;
   package TC renames AUnit.Test_Cases;

   type Atomics_Test is new
      TC.Test_Case with null record;

   overriding procedure Set_Up_Case (T: in out Atomics_Test);
   overriding procedure Tear_Down_Case (T: in out Atomics_Test);
   overriding procedure Set_Up (T: in out Atomics_Test);
   overriding procedure Tear_Down (T: in out Atomics_Test);

   procedure Register_Tests (T: in out Atomics_Test);
   function Name (T: in Atomics_Test) return AU.Message_String;

   -- Test Routines:
   procedure Test_Increment (T: in out TC.Test_Case'Class);
   procedure Test_Decrement (T: in out TC.Test_Case'Class);
   procedure Test_Safe_Decrement (T: in out TC.Test_Case'Class);
end Shared.Atomics.Tests;
