with AUnit.Assertions;

package body Interface_Layer.Wrappers.Application.Tests is
   package Ass renames AUnit.Assertions;

   -----------------------------------------------------
   --                   SCAFFOLDING
   -----------------------------------------------------

   procedure Set_Up (T: in out Application_Test) is
   begin
      null;
   end Set_Up;

   procedure Tear_Down (T: in out Application_Test) is
   begin
      null;
   end Tear_Down;

   -----------------------------------------------------
   --                   SCENARIOS
   -----------------------------------------------------

   procedure Test_Empty (T : in out TC.Test_Case'Class)
   is
   begin
      -- TODO: implement this test
      Ass.Assert (0=1,
                  "Test not yet implemented");
      null;
   end Test_Empty;

   procedure Test_Create_Ack (T : in out TC.Test_Case'Class)
   is
   begin
      -- TODO: implement this test
      Ass.Assert (0=1,
                  "Test not yet implemented");
      null;
   end Test_Create_Ack;

   procedure Test_Create_Message (T : in out TC.Test_Case'Class)
   is
   begin
      -- TODO: implement this test
      Ass.Assert (0=1,
                  "Test not yet implemented");
      null;
   end Test_Create_Message;

   procedure Test_Create_Traveller (T : in out TC.Test_Case'Class)
   is
   begin
      -- TODO: implement this test
      Ass.Assert (0=1,
                  "Test not yet implemented");
      null;
   end Test_Create_Traveller;

   procedure Test_Get_Data (T : in out TC.Test_Case'Class)
   is
   begin
      -- TODO: implement this test
      Ass.Assert (0=1,
                  "Test not yet implemented");
      null;
   end Test_Get_Data;

   procedure Test_Get_Concrete_Data_Type (T : in out TC.Test_Case'Class)
   is
   begin
      -- TODO: implement this test
      Ass.Assert (0=1,
                  "Test not yet implemented");
      null;
   end Test_Get_Concrete_Data_Type;

   procedure Test_Equals (T : in out TC.Test_Case'Class)
   is
   begin
      -- TODO: implement this test
      Ass.Assert (0=1,
                  "Test not yet implemented");
      null;
   end Test_Equals;

   procedure Test_Finalize_Ref (T : in out TC.Test_Case'Class)
   is
   begin
      -- TODO: implement this test
      Ass.Assert (0=1,
                  "Test not yet implemented");
      null;
   end Test_Finalize_Ref;

   procedure Test_Extract_Concrete_Data_Type (T : in out TC.Test_Case'Class)
   is
   begin
      -- TODO: implement this test
      Ass.Assert (0=1,
                  "Test not yet implemented");
      null;
   end Test_Extract_Concrete_Data_Type;


   -----------------------------------------------------
   --                  REGISTRATION
   -----------------------------------------------------
   procedure Register_Tests (T: in out Application_Test) is
      use TC.Registration;
   begin
      null;
      Register_Routine (Test => T,
                        Routine => Test_Empty'Access,
                        Name => "Test execute Empty");
      Register_Routine (Test => T,
                        Routine => Test_Create_Message'Access,
                        Name => "Test execute Create_Message");
      Register_Routine (Test => T,
                        Routine => Test_Create_Traveller'Access,
                        Name => "Test execute Create_Traveller");
      Register_Routine (Test => T,
                        Routine => Test_Get_Data'Access,
                        Name => "Test execute Get_Data");
      Register_Routine (Test => T,
                        Routine => Test_Get_Concrete_Data_Type'Access,
                        Name => "Test execute Get_Concrete_Data_Type");
      Register_Routine (Test => T,
                        Routine => Test_Equals'Access,
                        Name => "Test execute Equals");
      Register_Routine (Test => T,
                        Routine => Test_Finalize_Ref'Access,
                        Name => "Test execute Finalize_Ref");
      Register_Routine (Test => T,
                        Routine => Test_Extract_Concrete_Data_Type'Access,
                        Name => "Test execute Extract_Concrete_Data_Type");
   end Register_Tests;

   function Name(T: Application_Test) return AU.Message_String is
   begin
      return AU.Format ("Application");
   end Name;
end Interface_Layer.Wrappers.Application.Tests;