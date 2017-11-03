with AUnit;
with AUnit.Test_Cases;

package Reactive.Infrastructure.Build.Config_Reader.Tests is
   package AU renames AUnit;
   package TC renames AUnit.Test_Cases;

   type Config_Reader_Test is new TC.Test_Case with null record;

   overriding procedure Set_Up (T : in out Config_Reader_Test);

   -- Test Routines:
   procedure Test_Read_Hosts         (T: in out TC.Test_Case'Class);
   procedure Test_Read_Streets       (T: in out TC.Test_Case'Class);
   procedure Test_Read_Intersections (T: in out TC.Test_Case'Class);
   procedure Test_Read_Config        (T: in out TC.Test_Case'Class);

   procedure Register_Tests (T : in out Config_Reader_Test);
   overriding function Name (T : in Config_Reader_Test)
      return AU.Message_String;

end Reactive.Infrastructure.Build.Config_Reader.Tests;
