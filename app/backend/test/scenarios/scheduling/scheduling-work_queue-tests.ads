with AUnit;
with AUnit.Test_Cases;

package Scheduling.Work_Queue.Tests is
   package AU renames AUnit;
   package TC renames AUnit.Test_Cases;

   type Work_Queue_Test is new
      TC.Test_Case with null record;

   overriding procedure Set_Up (T: in out Work_Queue_Test);
   overriding procedure Tear_Down (T: in out Work_Queue_Test);

   procedure Register_Tests (T: in out Work_Queue_Test);
   function Name (T: in Work_Queue_Test) return AU.Message_String;

   -- Test Routines:
   procedure Test_Add_Item (T: in out TC.Test_Case'Class);
   procedure Test_Add_Items (T: in out TC.Test_Case'Class);
   procedure Test_Take_Item (T: in out TC.Test_Case'Class);
end Scheduling.Work_Queue.Tests;
