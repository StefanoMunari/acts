with AUnit;
with AUnit.Test_Cases;

package Reactive.Traveller_Registry.Tests is
   package AU renames AUnit;
   package TC renames AUnit.Test_Cases;

   type Traveller_Registry_Test is new
      TC.Test_Case with null record;

   overriding procedure Set_Up_Case (T: in out Traveller_Registry_Test);
   overriding procedure Tear_Down (T: in out Traveller_Registry_Test);

   procedure Register_Tests (T: in out Traveller_Registry_Test);
   overriding function Name (T: in Traveller_Registry_Test) return AU.Message_String;
   
   -- Test Routines:
   procedure Test_Traveller_Contains (T : in out TC.Test_Case'Class);
   procedure Test_Traveller_Find (T : in out TC.Test_Case'Class);
   procedure Test_Traveller_Add (T : in out TC.Test_Case'Class);
   procedure Test_Traveller_Remove (T : in out TC.Test_Case'Class);
   procedure Test_Remove_All_Travellers (T : in out TC.Test_Case'Class);

end Reactive.Traveller_Registry.Tests;
