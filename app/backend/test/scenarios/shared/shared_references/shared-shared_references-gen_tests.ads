with AUnit;
with AUnit.Test_Cases;
with AUnit.Test_Fixtures;

with Shared.Shared_References;

generic
   with procedure Initialize (Encapsulated_Ref : out T_Reference);
package Shared.Shared_References.Gen_Tests is
   package AU renames AUnit;
   package TC renames AUnit.Test_Cases;

   type Shared_References_Test is new AUnit.Test_Fixtures.Test_Fixture with private;

   overriding procedure Set_Up (TI : in out Shared_References_Test);
   overriding procedure Tear_Down (TI : in out Shared_References_Test);

   -- Test Routines:
   procedure Test_Init_Reference_Success (TI : in out Shared_References_Test);
   procedure Test_Init_Reference_Fail (TI : in out Shared_References_Test);
   procedure Test_Init_Shared_Record_Success (TI : in out Shared_References_Test);
   procedure Test_Init_Shared_Record_Fail (TI : in out Shared_References_Test);
   procedure Test_Share_Success (TI : in out Shared_References_Test);
   procedure Test_Share_Fail (TI : in out Shared_References_Test);
   procedure Test_Get_Success (TI : in out Shared_References_Test);
   procedure Test_Get_Fail (TI : in out Shared_References_Test);
   procedure Test_Get_Reference_Success (TI : in out Shared_References_Test);
   procedure Test_Get_Reference_Fail (TI : in out Shared_References_Test);
   procedure Test_Get_Counter_Zero (TI : in out Shared_References_Test);
   procedure Test_Get_Counter (TI : in out Shared_References_Test);
   procedure Test_Reference_Counting_3 (TI : in out Shared_References_Test);
   -- Concurrent tests
   procedure Test_Concurrent_Share (TI : in out Shared_References_Test);

private
   type Shared_References_Test is new AUnit.Test_Fixtures.Test_Fixture with
   record
      Encapsulated_Ref : T_Reference;
   end record;

   task type Shared_Ref_Task is
      entry Exec;
      entry Sync;
   end Shared_Ref_Task;

end Shared.Shared_References.Gen_Tests;
