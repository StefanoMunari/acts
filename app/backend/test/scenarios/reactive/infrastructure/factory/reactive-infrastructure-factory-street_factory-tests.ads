with AUnit;
with AUnit.Test_Cases;

package Reactive.Infrastructure.Factory.Street_Factory.Tests is
   package AU renames AUnit;
   package TC renames AUnit.Test_Cases;

   type Street_Factory_Test is new TC.Test_Case with null record;

   overriding procedure Set_Up (T : in out Street_Factory_Test);

   -- Test Routines:
   procedure Test_Decorate_Stretch_No_Decoration (T: in out TC.Test_Case'Class);
   procedure Test_Decorate_Stretch_Ped_Crossing  (T: in out TC.Test_Case'Class);
   procedure Test_Decorate_Stretch_Bike_Crossing (T: in out TC.Test_Case'Class);
   procedure Test_Decorate_Stretch_Bus_Stop      (T: in out TC.Test_Case'Class);
   procedure Test_Decorate_Stretch_Facility      (T: in out TC.Test_Case'Class);
   procedure Test_Decorate_Lane_No_Decoration    (T: in out TC.Test_Case'Class);
   procedure Test_Decorate_Lane_Speed_Limit      (T: in out TC.Test_Case'Class);

-- TODO: This tests may be refined in order to actually check if factory set
--+      exact values for decorations

-- TODO: Test multiple decoration stacked one over the other

-- TODO: More complex tests for facility decoration?
--+      e.g., 1) another stretch has already been created and has a parking
--+      e.g., 2) no other stretch has already been created => no parking

   procedure Register_Tests (T : in out Street_Factory_Test);
   overriding function Name (T : in Street_Factory_Test)
      return AU.Message_String;

end Reactive.Infrastructure.Factory.Street_Factory.Tests;
