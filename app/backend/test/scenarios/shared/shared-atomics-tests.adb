with AUnit.Assertions;
with Shared.Atomics;
-- core
with Ada.Unchecked_Deallocation;

package body Shared.Atomics.Tests is
   package Ass renames AUnit.Assertions;
   package Atomics renames Shared.Atomics;

   -----------------------------------------------------
   --                   SCAFFOLDING
   -----------------------------------------------------
   type Atomic_Counter_Ptr is access Shared.Atomics.Atomic_Counter;
   Atomic_Counter_To_Test0 : Atomic_Counter_Ptr;

   procedure Set_Up_Case (T: in out Atomics_Test) is
   begin
    Atomic_Counter_To_Test0 := new Atomics.Atomic_Counter;
   end Set_Up_Case;

   procedure Set_Up (T: in out Atomics_Test) is
   begin
    null;
   end Set_Up;

   procedure Tear_Down (T: in out Atomics_Test) is
   begin
    null;
   end Tear_Down;

   procedure Tear_Down_Case (T: in out Atomics_Test) is
      procedure Free is
        new Ada.Unchecked_Deallocation
        (Shared.Atomics.Atomic_Counter, Atomic_Counter_Ptr);
   begin
    Free (Atomic_Counter_To_Test0);
   end Tear_Down_Case;

   -----------------------------------------------------
   --                   SCENARIOS
   -----------------------------------------------------
   procedure Test_Increment (T: in out TC.Test_Case'Class)
   is
      Expected_Result : Natural := 1;
   begin
      Atomic_Counter_To_Test0.all.Increment;
      Ass.Assert ((Expected_Result = Atomic_Counter_To_Test0.all.Get),
                   "Does not increment to 1");
   end Test_Increment;

   procedure Test_Decrement (T: in out TC.Test_Case'Class)
   is
      Expected_Result : Natural := 0;
   begin
      Atomic_Counter_To_Test0.all.Decrement;
      Ass.Assert ((Expected_Result = Atomic_Counter_To_Test0.all.Get),
                   "Does not decrement to 0");
   end Test_Decrement;

   procedure Test_Safe_Decrement (T: in out TC.Test_Case'Class)
   is
      Expected_Result : Natural := 0;
   begin
      Atomic_Counter_To_Test0.all.Decrement;
      Ass.Assert ((Expected_Result = Atomic_Counter_To_Test0.all.Get),
                   "Does not safe-decrement to 0");
   end Test_Safe_Decrement;

   -----------------------------------------------------
   --                  REGISTRATION
   -----------------------------------------------------
   procedure Register_Tests (T : in out Atomics_Test) is
      use TC.Registration;
   begin
      Register_Routine (Test => T,
                        Routine => Test_Increment'Access,
                        Name => "Test_Increment");
      Register_Routine (Test => T,
                        Routine => Test_Decrement'Access,
                        Name => "Test_Decrement");
      Register_Routine (Test => T,
                        Routine => Test_Safe_Decrement'Access,
                        Name => "Test_Safe_Decrement");
   end Register_Tests;

   function Name(T : Atomics_Test) return AU.Message_String is
   begin
      return AU.Format ("Shared.Atomics");
   end Name;
end Shared.Atomics.Tests;