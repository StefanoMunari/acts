with AUnit;
with AUnit.Test_Cases;

package Interface_Layer.Wrappers.Application.Tests is
   package AU renames AUnit;
   package TC renames AUnit.Test_Cases;

   type Application_Test is new
      TC.Test_Case with null record;

   overriding procedure Set_Up (T: in out Application_Test);
   overriding procedure Tear_Down (T: in out Application_Test);

   procedure Register_Tests (T: in out Application_Test);
   function Name (T: in Application_Test) return AU.Message_String;

   -- Test Routines:
   procedure Test_Empty (T: in out TC.Test_Case'Class);
   procedure Test_Create_Ack (T: in out TC.Test_Case'Class);
   procedure Test_Create_Message (T: in out TC.Test_Case'Class);
   procedure Test_Create_Traveller (T: in out TC.Test_Case'Class);
   procedure Test_Get_Data (T: in out TC.Test_Case'Class);
   procedure Test_Get_Concrete_Data_Type (T: in out TC.Test_Case'Class);
   procedure Test_Equals (T: in out TC.Test_Case'Class);
   procedure Test_Finalize_Ref (T: in out TC.Test_Case'Class);
   procedure Test_Extract_Concrete_Data_Type (T: in out TC.Test_Case'Class);

end Interface_Layer.Wrappers.Application.Tests;