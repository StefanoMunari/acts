with AUnit;
with AUnit.Test_Cases;

package Reactive.Traffic_Light_Registry.Tests is
   package AU renames AUnit;
   package TC renames AUnit.Test_Cases;

   type Traffic_Light_Registry_Test is new
      TC.Test_Case with null record;

   overriding procedure Set_Up_Case (T: in out Traffic_Light_Registry_Test);
   overriding procedure Tear_Down (T: in out Traffic_Light_Registry_Test);

   procedure Register_Tests (T: in out Traffic_Light_Registry_Test);
   overriding function Name (T: in Traffic_Light_Registry_Test)
      return AU.Message_String;
   
   -- Test Routines:
   procedure Test_Traffic_Light_Contains (T : in out TC.Test_Case'Class);
   procedure Test_Traffic_Light_Find (T : in out TC.Test_Case'Class);
   procedure Test_Traffic_Light_Add (T : in out TC.Test_Case'Class);
   procedure Test_Traffic_Light_Remove (T : in out TC.Test_Case'Class);
   procedure Test_Remove_All_Traffic_Lights (T : in out TC.Test_Case'Class);

end Reactive.Traffic_Light_Registry.Tests;
