with AUnit;
with AUnit.Test_Cases;

package Reactive.Infrastructure.Build.Street_Builder.Tests is
   package AU renames AUnit;
   package TC renames AUnit.Test_Cases;

   type Street_Builder_Test is new TC.Test_Case with null record;

   overriding procedure Set_Up (T : in out Street_Builder_Test);

   -- Test Routines:
   procedure Test_Get_Stretches_Empty (T: in out TC.Test_Case'Class);
   procedure Test_Get_Stretches_One (T: in out TC.Test_Case'Class);
   procedure Test_Get_Stretches_Multi (T: in out TC.Test_Case'Class);
   procedure Test_Get_Lanes_Empty (T: in out TC.Test_Case'Class);
   procedure Test_Get_Lanes_One (T: in out TC.Test_Case'Class);
   procedure Test_Get_Lanes_Multi (T: in out TC.Test_Case'Class);
   procedure Test_With_Bikeway (T: in out TC.Test_Case'Class);
   procedure Test_With_Footway (T: in out TC.Test_Case'Class);
   procedure Test_With_Roadway (T: in out TC.Test_Case'Class);
   procedure Test_Get_Street (T: in out TC.Test_Case'Class);

   procedure Register_Tests (T : in out Street_Builder_Test);
   overriding function Name (T : in Street_Builder_Test)
      return AU.Message_String;

end Reactive.Infrastructure.Build.Street_Builder.Tests;
