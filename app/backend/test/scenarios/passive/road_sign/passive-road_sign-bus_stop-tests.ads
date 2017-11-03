with AUnit;
with AUnit.Test_Cases;

package Passive.Road_Sign.Bus_Stop.Tests is
   package AU renames AUnit;
   package TC renames AUnit.Test_Cases;

   type Bus_Stop_Test is new
      TC.Test_Case with null record;

   overriding procedure Set_Up_Case (T: in out Bus_Stop_Test);
   overriding procedure Tear_Down_Case (T: in out Bus_Stop_Test);
   overriding procedure Set_Up (T: in out Bus_Stop_Test);
   overriding procedure Tear_Down (T: in out Bus_Stop_Test);

   procedure Register_Tests (T: in out Bus_Stop_Test);
   function Name (T: in Bus_Stop_Test) return AU.Message_String;

   -- Test Routines:
   procedure Bus_Service_On_Bus_Stop (T: in out TC.Test_Case'Class);
end Passive.Road_Sign.Bus_Stop.Tests;
