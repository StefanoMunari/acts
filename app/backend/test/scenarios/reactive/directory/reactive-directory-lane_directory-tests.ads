with AUnit;
with AUnit.Test_Cases;

package Reactive.Directory.Lane_Directory.Tests is
   package AU renames AUnit;
   package TC renames AUnit.Test_Cases;

   type Lane_Directory_Test is new
      TC.Test_Case with null record;

   overriding procedure Set_Up (T: in out Lane_Directory_Test);
   overriding procedure Tear_Down (T: in out Lane_Directory_Test);

   procedure Register_Tests (T: in out Lane_Directory_Test);
   overriding function Name (T: in Lane_Directory_Test)
      return AU.Message_String;
   
   -- Test Routines:
   procedure Test_Add (T : in out TC.Test_Case'Class);
   procedure Test_Contains_Infrastructure (T : in out TC.Test_Case'Class);
   procedure Test_Find_By_Id (T : in out TC.Test_Case'Class);
   procedure Test_Safe_Find_By_Id (T : in out TC.Test_Case'Class);

end Reactive.Directory.Lane_Directory.Tests;
