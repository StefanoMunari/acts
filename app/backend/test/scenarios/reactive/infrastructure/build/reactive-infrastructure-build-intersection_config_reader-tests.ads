with AUnit;
with AUnit.Test_Cases;

package Reactive.Infrastructure.Build.Intersection_Config_Reader.Tests is
   package AU renames AUnit;
   package TC renames AUnit.Test_Cases;

   type Intersection_Config_Reader_Test is new TC.Test_Case with null record;

   overriding procedure Set_Up (T : in out Intersection_Config_Reader_Test);

   -- Test Routines:
   procedure Test_Set_Builder (T: in out TC.Test_Case'Class);
   procedure Test_Read_T (T: in out TC.Test_Case'Class);
   procedure Test_Read_X (T: in out TC.Test_Case'Class);

   procedure Register_Tests (T : in out Intersection_Config_Reader_Test);
   overriding function Name (T : in Intersection_Config_Reader_Test)
      return AU.Message_String;

end Reactive.Infrastructure.Build.Intersection_Config_Reader.Tests;
