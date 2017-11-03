with AUnit;
with AUnit.Test_Cases;

package Interface_Layer.Session.Senders.Tests is
   package AU renames AUnit;
   package TC renames AUnit.Test_Cases;

   type Senders_Test is new
      TC.Test_Case with null record;

   overriding procedure Set_Up (T: in out Senders_Test);
   overriding procedure Tear_Down (T: in out Senders_Test);
   overriding procedure Tear_Down_Case (T: in out Senders_Test);

   procedure Register_Tests (T: in out Senders_Test);
   function Name (T: in Senders_Test) return AU.Message_String;

   -- Test Routines:
   procedure Test_Init (T: in out TC.Test_Case'Class);
   procedure Test_Start (T: in out TC.Test_Case'Class);
   procedure Test_Shutdown (T: in out TC.Test_Case'Class);

end Interface_Layer.Session.Senders.Tests;