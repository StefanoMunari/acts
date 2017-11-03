with AUnit;
with AUnit.Test_Cases;

package Interface_Layer.Session.Receivers.Tests is
   package AU renames AUnit;
   package TC renames AUnit.Test_Cases;

   type Receivers_Test is new
      TC.Test_Case with null record;

   overriding procedure Set_Up (T: in out Receivers_Test);
   overriding procedure Tear_Down (T: in out Receivers_Test);
   overriding procedure Tear_Down_Case (T: in out Receivers_Test);

   procedure Register_Tests (T: in out Receivers_Test);
   function Name (T: in Receivers_Test) return AU.Message_String;

   -- Test Routines:
   procedure Test_Init (T: in out TC.Test_Case'Class);
   procedure Test_Start (T: in out TC.Test_Case'Class);
   procedure Test_Shutdown (T: in out TC.Test_Case'Class);

end Interface_Layer.Session.Receivers.Tests;