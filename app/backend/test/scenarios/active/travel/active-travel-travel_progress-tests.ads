with AUnit;
with AUnit.Test_Cases;

package Active.Travel.Travel_Progress.Tests is
   package AU renames AUnit;
   package TC renames AUnit.Test_Cases;

   type Travel_Progress_Test is new TC.Test_Case with null record;

   overriding procedure Set_Up (T: in out Travel_Progress_Test);

   -- Test Routines:
   procedure Test_Has_Next_Step (T: in out TC.Test_Case'Class);
   procedure Test_Is_Progressing (T: in out TC.Test_Case'Class);
   procedure Test_Advance (T : in out TC.Test_Case'Class);

   procedure Register_Tests (T: in out Travel_Progress_Test);
   overriding function Name (T: in Travel_Progress_Test) return AU.Message_String;

end Active.Travel.Travel_Progress.Tests;
