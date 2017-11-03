with AUnit.Assertions;

package body Interface_Layer.Presentation.Tests is
   package Ass renames AUnit.Assertions;

   -----------------------------------------------------
   --                   SCAFFOLDING
   -----------------------------------------------------

   procedure Set_Up (T: in out Presentation_Test) is
   begin
      null;
   end Set_Up;

   procedure Tear_Down (T: in out Presentation_Test) is
   begin
      null;
   end Tear_Down;

   -----------------------------------------------------
   --                   SCENARIOS
   -----------------------------------------------------

   procedure Test_Init (T : in out TC.Test_Case'Class)
   is
   begin
      -- TODO: implement this test
      Ass.Assert (0=1,
                  "Test not yet implemented");
      null;
   end Test_Init;

   procedure Test_Shutdown (T : in out TC.Test_Case'Class)
   is
   begin
      -- TODO: implement this test
      Ass.Assert (0=1,
                  "Test not yet implemented");
      null;
   end Test_Shutdown;

   -----------------------------------------------------
   --                  REGISTRATION
   -----------------------------------------------------
   procedure Register_Tests (T: in out Presentation_Test) is
      use TC.Registration;
   begin
      null;
      Register_Routine (Test => T,
                        Routine => Test_Init'Access,
                        Name => "Test execute Init");
      Register_Routine (Test => T,
                        Routine => Test_Shutdown'Access,
                        Name => "Test execute Shutdown");
   end Register_Tests;

   function Name(T: Presentation_Test) return AU.Message_String is
   begin
      return AU.Format ("Presentation");
   end Name;
end Interface_Layer.Presentation.Tests;