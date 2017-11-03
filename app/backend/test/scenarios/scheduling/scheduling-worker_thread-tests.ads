with AUnit;
with AUnit.Test_Cases;

package Scheduling.Worker_Thread.Tests is
   package AU renames AUnit;
   package TC renames AUnit.Test_Cases;

   type Worker_Thread_Test is new
      TC.Test_Case with null record;

   overriding procedure Set_Up (T: in out Worker_Thread_Test);
   overriding procedure Tear_Down (T: in out Worker_Thread_Test);

   procedure Register_Tests (T: in out Worker_Thread_Test);
   function  Name (T: in Worker_Thread_Test) return AU.Message_String;

   -- Test Routines:
   procedure Test_Can_Stop             (T: in out TC.Test_Case'Class);
   procedure Test_Can_Execute_And_Stop (T: in out TC.Test_Case'Class);
end Scheduling.Worker_Thread.Tests;
