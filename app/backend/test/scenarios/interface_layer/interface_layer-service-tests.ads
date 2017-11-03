with AUnit;
with AUnit.Test_Cases;

package Interface_Layer.Service.Tests is
   package AU renames AUnit;
   package TC renames AUnit.Test_Cases;

   type Service_Test is new
      TC.Test_Case with null record;

   overriding procedure Set_Up (T: in out Service_Test);
   overriding procedure Tear_Down (T: in out Service_Test);

   procedure Register_Tests (T: in out Service_Test);
   function Name (T: in Service_Test) return AU.Message_String;

   -- Test Routines:
   procedure Test_Init (T: in out TC.Test_Case'Class);
   procedure Test_Shutdown (T: in out TC.Test_Case'Class);

end Interface_Layer.Service.Tests;