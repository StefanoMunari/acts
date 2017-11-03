with AUnit;
with AUnit.Test_Cases;

package Active.Travel.Travel_Planning.Tests is
   package AU renames AUnit;
   package TC renames AUnit.Test_Cases;

   type Travel_Planning_Test is new TC.Test_Case with null record;

   overriding procedure Set_Up (T: in out Travel_Planning_Test);

   -- Test Routines:
   procedure Test_Has_No_Next_Step (T: in out TC.Test_Case'Class);
   procedure Test_Is_Not_Progressing (T: in out TC.Test_Case'Class);
   procedure Test_Plan_The_Same_Stretch (T : in out TC.Test_Case'Class);

   procedure Register_Tests (T: in out Travel_Planning_Test);
   overriding function Name (T: in Travel_Planning_Test) return AU.Message_String;

end Active.Travel.Travel_Planning.Tests;
