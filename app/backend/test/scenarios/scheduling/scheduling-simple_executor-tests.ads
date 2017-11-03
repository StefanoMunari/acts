with AUnit;
with AUnit.Test_Cases;

package Scheduling.Simple_Executor.Tests is
   package AU renames AUnit;
   package TC renames AUnit.Test_Cases;

   type Simple_Executor_Test is new TC.Test_Case with null record;

   overriding procedure Set_Up (T: in out Simple_Executor_Test);
   overriding procedure Tear_Down (T: in out Simple_Executor_Test);

   procedure Register_Tests (T: in out Simple_Executor_Test);
   function Name (T: in Simple_Executor_Test) return AU.Message_String;

   -- Test Routines:
   procedure Test_Execute_One (T: in out TC.Test_Case'Class);
   procedure Test_Execute_More (T: in out TC.Test_Case'Class);
end Scheduling.Simple_Executor.Tests;
