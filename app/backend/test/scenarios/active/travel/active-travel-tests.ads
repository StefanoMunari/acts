with AUnit;
with AUnit.Test_Cases;

package Active.Travel.Tests is
   package AU renames AUnit;
   package TC renames AUnit.Test_Cases;

   type Travel_Test is new TC.Test_Case with null record;

   overriding procedure Set_Up (T : in out Travel_Test);

   -- Test Routines:
   procedure Test_Has_Next_Step (T : in out TC.Test_Case'Class);
   procedure Test_Has_No_Next_Step (T : in out TC.Test_Case'Class);
   procedure Test_Is_Progressing (T : in out TC.Test_Case'Class);
   procedure Test_Is_Not_Progressing (T : in out TC.Test_Case'Class);
   procedure Test_Route_Source_Id_Getter (T : in out TC.Test_Case'Class);
   procedure Test_Route_Destination_Id_Getter (T : in out TC.Test_Case'Class);
   procedure Test_Traveller_Id_Getter (T : in out TC.Test_Case'Class);
   procedure Test_Current_Step_Getter (T : in out TC.Test_Case'Class);
   procedure Test_Next_Step_Getter (T : in out TC.Test_Case'Class);
   procedure Test_Previous_Step_Getter (T : in out TC.Test_Case'Class);
   procedure Test_Step_Prepend (T : in out TC.Test_Case'Class);
   procedure Test_Travel_State_Change (T : in out TC.Test_Case'Class);
--     procedure Test_Advance (T : in out TC.Test_Case'Class);
--     procedure Test_Not_Advance (T : in out TC.Test_Case'Class);
   procedure Test_First_Step_Id_Getter (T : in out TC.Test_Case'Class);
   procedure Test_Last_Step_Id_Getter (T : in out TC.Test_Case'Class);
   procedure Test_Route_Getter (T : in out TC.Test_Case'Class);
   procedure Test_Route_Clear (T : in out TC.Test_Case'Class);

   procedure Register_Tests (T : in out Travel_Test);
   overriding function Name (T : in Travel_Test) return AU.Message_String;

end Active.Travel.Tests;
