with AUnit;
with AUnit.Test_Cases;

package Reactive.Infrastructure_Registry.Tests is
   package AU renames AUnit;
   package TC renames AUnit.Test_Cases;

   type Infrastructure_Registry_Test is new
      TC.Test_Case with null record;

   overriding procedure Set_Up_Case (T: in out Infrastructure_Registry_Test);
   overriding procedure Tear_Down (T: in out Infrastructure_Registry_Test);

   procedure Register_Tests (T: in out Infrastructure_Registry_Test);
   overriding function Name (T: in Infrastructure_Registry_Test) return AU.Message_String;

   -- Test Routines:
   procedure Test_Infrastructure_Contains (T : in out TC.Test_Case'Class);
   procedure Test_Infrastructure_Find (T : in out TC.Test_Case'Class);
   procedure Test_Intersection_Find (T : in out TC.Test_Case'Class);
   procedure Test_Street_Find (T : in out TC.Test_Case'Class);
   procedure Test_Roadway_Find (T : in out TC.Test_Case'Class);
   procedure Test_Footway_Find (T : in out TC.Test_Case'Class);
   procedure Test_Bikeway_Find (T : in out TC.Test_Case'Class);
   procedure Test_Lane_Find (T : in out TC.Test_Case'Class);
   procedure Test_Stretch_Find (T : in out TC.Test_Case'Class);
   procedure Test_Remove_All_Infrastructures (T : in out TC.Test_Case'Class);

end Reactive.Infrastructure_Registry.Tests;
