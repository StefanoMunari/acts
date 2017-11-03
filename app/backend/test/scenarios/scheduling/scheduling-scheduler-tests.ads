with AUnit;
with AUnit.Test_Cases;

package Scheduling.Scheduler.Tests is
   package AU renames AUnit;
   package TC renames AUnit.Test_Cases;

   type Scheduler_Test is new
      TC.Test_Case with null record;

   overriding procedure Set_Up (T: in out Scheduler_Test);
   overriding procedure Set_Up_Case (T: in out Scheduler_Test);
   overriding procedure Tear_Down (T: in out Scheduler_Test);

   procedure Register_Tests (T: in out Scheduler_Test);
   function Name (T: in Scheduler_Test) return AU.Message_String;

   -- Test Routines:
   procedure Test_Schedule (T: in out TC.Test_Case'Class);
end Scheduling.Scheduler.Tests;
