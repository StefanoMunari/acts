with AUnit;
with AUnit.Test_Cases;

package Shared.Rendezvous.Boolean_Rendezvous.Tests is
   package AU renames AUnit;
   package TC renames AUnit.Test_Cases;

   type BR_Test is new
      TC.Test_Case with null record;

   overriding procedure Set_Up_Case (T: in out BR_Test);
   overriding procedure Tear_Down_Case (T: in out BR_Test);
   overriding procedure Set_Up (T: in out BR_Test);
   overriding procedure Tear_Down (T: in out BR_Test);

   procedure Register_Tests (T: in out BR_Test);
   function Name (T: in BR_Test) return AU.Message_String;

   -- Test Routines:
   procedure Test_Equality (T: in out TC.Test_Case'Class);
   procedure Test_Disequality (T: in out TC.Test_Case'Class);

end Shared.Rendezvous.Boolean_Rendezvous.Tests;
