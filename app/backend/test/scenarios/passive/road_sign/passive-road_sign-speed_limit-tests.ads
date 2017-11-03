with AUnit;
with AUnit.Test_Cases;

package Passive.Road_Sign.Speed_Limit.Tests is
   package AU renames AUnit;
   package TC renames AUnit.Test_Cases;

   type Speed_Limit_Test is new
      TC.Test_Case with null record;

   overriding procedure Set_Up_Case (T: in out Speed_Limit_Test);
   overriding procedure Tear_Down_Case (T: in out Speed_Limit_Test);
   overriding procedure Set_Up (T: in out Speed_Limit_Test);
   overriding procedure Tear_Down (T: in out Speed_Limit_Test);

   procedure Register_Tests (T: in out Speed_Limit_Test);
   function Name (T: in Speed_Limit_Test) return AU.Message_String;

   -- Test Routines:
   procedure Speed_Limit_Under_The_Limit (T: in out TC.Test_Case'Class);
   procedure Speed_Limit_Equal_To_Limit  (T: in out TC.Test_Case'Class);
   procedure Speed_Limit_Over_The_Limit  (T: in out TC.Test_Case'Class);

end Passive.Road_Sign.Speed_Limit.Tests;
