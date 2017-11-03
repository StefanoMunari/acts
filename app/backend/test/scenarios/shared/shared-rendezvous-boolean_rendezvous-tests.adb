with AUnit.Assertions;

package body Shared.Rendezvous.Boolean_Rendezvous.Tests is
   package Ass renames AUnit.Assertions;

   -----------------------------------------------------
   --                   SCAFFOLDING
   -----------------------------------------------------
   procedure Set_Up_Case (T: in out BR_Test) is
   begin
      null;
   end Set_Up_Case;

   procedure Tear_Down_Case (T: in out BR_Test) is
   begin
      null;
   end Tear_Down_Case;

   procedure Set_Up (T: in out BR_Test) is
   begin
      null;
   end Set_Up;

   procedure Tear_Down (T: in out BR_Test) is
   begin
      null;
   end Tear_Down;

   -----------------------------------------------------
   --                   SCENARIOS
   -----------------------------------------------------
   procedure Test_Equality (T: in out TC.Test_Case'Class)
   is
      Rendezvous_A : Boolean_Rendezvous.Reference
         := new Boolean_Rendezvous.Blocking_Rendezvous;
      Rendezvous_B : Boolean_Rendezvous.Reference := Rendezvous_A;
   begin
      Ass.Assert (Rendezvous_A = Rendezvous_B, "A /= B");
   end Test_Equality;

   procedure Test_Disequality (T: in out TC.Test_Case'Class)
   is
      Rendezvous_A : Boolean_Rendezvous.Reference
         := new Boolean_Rendezvous.Blocking_Rendezvous;
      Rendezvous_B : Boolean_Rendezvous.Reference
         := new Boolean_Rendezvous.Blocking_Rendezvous;
   begin
      Ass.Assert (not (Rendezvous_A = Rendezvous_B), "A = B");
   end Test_Disequality;

   -----------------------------------------------------
   --                  REGISTRATION 
   -----------------------------------------------------
   procedure Register_Tests (T: in out BR_Test) is
      use TC.Registration;
   begin
      Register_Routine (Test    => T,
                        Routine => Test_Equality'Access,
                        Name    => "Equality");
      Register_Routine (Test    => T,
                        Routine => Test_Disequality'Access,
                        Name    => "Test_Disequality");
   end Register_Tests;

   function Name(T: BR_Test) return AU.Message_String is
   begin
      return AU.Format ("Shared.Rendezvous.Boolean_Rendezvous");
   end Name;

end Shared.Rendezvous.Boolean_Rendezvous.Tests;
