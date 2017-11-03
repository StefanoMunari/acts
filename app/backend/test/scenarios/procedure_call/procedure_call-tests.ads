with AUnit;
with AUnit.Test_Cases;

package Procedure_Call.Tests is
   package AU renames AUnit;
   package TC renames AUnit.Test_Cases;

   type Procedure_Call_Test is new 
      TC.Test_Case with null record;

   overriding procedure Set_Up_Case (T: in out Procedure_Call_Test);
   overriding procedure Tear_Down_Case (T: in out Procedure_Call_Test);
   overriding procedure Set_Up (T: in out Procedure_Call_Test);
   overriding procedure Tear_Down (T: in out Procedure_Call_Test);

   procedure Register_Tests (T: in out Procedure_Call_Test);
   function Name (T: in Procedure_Call_Test) return AU.Message_String;

   -- Test Routines:
   procedure ID_Hashed (T: in out TC.Test_Case'Class);
end Procedure_Call.Tests;
