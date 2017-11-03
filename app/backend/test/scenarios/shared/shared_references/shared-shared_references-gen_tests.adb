-- lib
with AUnit.Assertions;
-- core
with Ada.Unchecked_Deallocation;
with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Task_Identification;

package body Shared.Shared_References.Gen_Tests is
   package IO renames Ada.Text_IO;
   package Ass renames AUnit.Assertions;
   package SR renames Shared.Shared_References;
   package SU renames Ada.Strings.Unbounded;
   -----------------------------------------------------
   --                   ON-THE-FLY MOCK ASYNC TASK
   -----------------------------------------------------
   SR_Ref : SR.Shared_Reference;

   task body Shared_Ref_Task is
      SR0 : Shared_Reference;
   begin
      accept Exec;
         Ada.Text_IO.Put_Line (Ada.Task_Identification.Image (Shared_Ref_Task'Identity) & " Executing ... ");
         SR0.Init (SR_Ref.Share);
         Ada.Text_IO.Put_Line (Ada.Task_Identification.Image (Shared_Ref_Task'Identity) & "::Counter [Concurrent] = " & Natural'Image (SR0.Get_Counter));
      accept Sync do
         Ass.Assert ((SR0.Get_Counter = SR_Ref.Get_Counter),
                   "Reference counting is not thread-safe");
         Ada.Text_IO.Put_Line (Ada.Task_Identification.Image (Shared_Ref_Task'Identity) & " Joined with Main");
      end Sync;
   end Shared_Ref_Task;

   -----------------------------------------------------
   --                   SCAFFOLDING
   -----------------------------------------------------

   procedure Set_Up (TI: in out Shared_References_Test) is
   begin
      Initialize (TI.Encapsulated_Ref);
   end Set_Up;

   procedure Tear_Down (TI: in out Shared_References_Test) is
   begin
      -- DO NOT FREE Shared_References_Test: the shared_reference itself will
      -- handle this reference deallocation automatically. Otherwise a
      -- STORAGE_ERROR will be raised by the compiler
      null;
   end Tear_Down;

   -----------------------------------------------------
   --                   SCENARIOS
   -----------------------------------------------------
   procedure Test_Init_Reference_Success (TI : in out Shared_References_Test)
   is
      Test_Shared_R : SR.Shared_Reference;
      Expected_Result : T := T (TI.Encapsulated_Ref.all);
      Expected_Result_Reference : T_Reference := TI.Encapsulated_Ref;
      Expected_Result_Counter : Natural := 1;
      Failed : Boolean := False;
   begin
      Test_Shared_R.Init (TI.Encapsulated_Ref);
      Ass.Assert ((Expected_Result = Test_Shared_R.Get),
                   "Not equal objects referenced by Encapsulated_References");
      Ass.Assert ((Expected_Result_Reference = Test_Shared_R.Get_Reference),
                   "Not equal Encapsulated_References");
      Ass.Assert ((Expected_Result_Counter = Test_Shared_R.Get_Counter),
                   "Wrong increment for the internal Reference_Counter");
      if (Test_Shared_R.Share = null) then
        null;
      end if;
      exception
         when CONSTRAINT_ERROR =>
            Failed := True;
      Ass.Assert (not(Failed), "Does not initialize the internal Wrapper");
   end Test_Init_Reference_Success;

   procedure Test_Init_Reference_Fail (TI : in out Shared_References_Test)
   is
      procedure Free is
        new Ada.Unchecked_Deallocation (T'Class, T_Reference);
      Failed : Boolean := False;
      Test_Shared_R : SR.Shared_Reference;
   begin
      Free (TI.Encapsulated_Ref);
      Test_Shared_R.Init (TI.Encapsulated_Ref);
      exception
         when CONSTRAINT_ERROR =>
            Failed := True;
      Ass.Assert ((Failed = True), "Does not raise CONSTRAINT_ERROR");
   end Test_Init_Reference_Fail;

   procedure Test_Init_Shared_Record_Success (TI : in out Shared_References_Test)
   is
      Test_Shared_R : SR.Shared_Reference;
      Valid_Reference : SR.Shared_Record_Reference := NULL;
   begin
      Test_Shared_R.Init (TI.Encapsulated_Ref);
      Valid_Reference := Test_Shared_R.Share;
      Ass.Assert ((Valid_Reference = Test_Shared_R.Share),
                   "Not equal shared records referenced by wrappers");
   end Test_Init_Shared_Record_Success;

   procedure Test_Init_Shared_Record_Fail (TI : in out Shared_References_Test)
   is
      Test_Shared_R : SR.Shared_Reference;
      Null_Reference : SR.Shared_Record_Reference := NULL;
      Failed : Boolean := False;
   begin
      Test_Shared_R.Init (Null_Reference);
      exception
         when CONSTRAINT_ERROR =>
            Failed := True;
      Ass.Assert ((Failed = True), "Does not raise CONSTRAINT_ERROR");
   end Test_Init_Shared_Record_Fail;


   procedure Test_Share_Success (TI : in out Shared_References_Test)
   is
      Test_Shared_R : SR.Shared_Reference;
      Valid_Reference : SR.Shared_Record_Reference := NULL;
   begin
      Test_Shared_R.Init (TI.Encapsulated_Ref);
      Valid_Reference := Test_Shared_R.Share;
      Ass.Assert (not(Valid_Reference = null), "Wrong shared record initialization");
      Ass.Assert ((Valid_Reference = Test_Shared_R.Share), "Not equal shared records");
   end Test_Share_Success;

   procedure Test_Share_Fail (TI : in out Shared_References_Test)
   is
      Test_Shared_R : SR.Shared_Reference;
      Null_Reference : SR.Shared_Record_Reference := NULL;
      Failed : Boolean := False;
   begin
      Null_Reference := Test_Shared_R.Share;
      exception
         when CONSTRAINT_ERROR =>
            Failed := True;
      Ass.Assert ((Failed = True), "Does not raise CONSTRAINT_ERROR");
   end Test_Share_Fail;

   procedure Test_Get_Success (TI : in out Shared_References_Test)
   is
      Test_Shared_R : SR.Shared_Reference;
      Result : T :=  T (TI.Encapsulated_Ref.all);
   begin
      Test_Shared_R.Init (TI.Encapsulated_Ref);
      Ass.Assert ((Result = Test_Shared_R.Get), "Not equal objects");
   end Test_Get_Success;

   procedure Test_Get_Fail (TI : in out Shared_References_Test)
   is
      Test_Shared_R : SR.Shared_Reference;
      Failed : Boolean := False;
   begin
      declare
         Result : T := Test_Shared_R.Get;
      begin
         null;
      end;
      exception
         when CONSTRAINT_ERROR =>
            Failed := True;
      Ass.Assert ((Failed = True), "Does not raise CONSTRAINT_ERROR");
   end Test_Get_Fail;

   procedure Test_Get_Reference_Success (TI : in out Shared_References_Test)
   is
      Test_Shared_R : SR.Shared_Reference;
      Result : T_Reference;
   begin
      Test_Shared_R.Init (TI.Encapsulated_Ref);
      Result := Test_Shared_R.Get_Reference;
      Ass.Assert ((Result = TI.Encapsulated_Ref), "Not equal references");
   end Test_Get_Reference_Success;

   procedure Test_Get_Reference_Fail (TI : in out Shared_References_Test)
   is
      Test_Shared_R : SR.Shared_Reference;
      Null_Reference : T_Reference := NULL;
      Failed : Boolean := False;
   begin
      Null_Reference := Test_Shared_R.Get_Reference;
      exception
         when CONSTRAINT_ERROR =>
            Failed := True;
      Ass.Assert ((Failed = True), "Does not raise CONSTRAINT_ERROR");
   end Test_Get_Reference_Fail;

   procedure Test_Get_Counter_Zero (TI : in out Shared_References_Test)
   is
      Test_Shared_R : SR.Shared_Reference;
      Result : Natural := 0;
   begin
      Ass.Assert ((Result = Test_Shared_R.Get_Counter), "Not equal counters");
   end Test_Get_Counter_Zero;

   procedure Test_Get_Counter (TI : in out Shared_References_Test)
   is
      Test_Shared_R : SR.Shared_Reference;
      Result : Natural := 1;
   begin
      Test_Shared_R.Init (TI.Encapsulated_Ref);
      Ass.Assert ((Result = Test_Shared_R.Get_Counter), "Not equal counters");
   end Test_Get_Counter;

   procedure Test_Reference_Counting_3 (TI : in out Shared_References_Test)
   is
      Test_Shared_R : SR.Shared_Reference;
      Result : Natural := 0;
   begin
      Ass.Assert ((Result = Test_Shared_R.Get_Counter), "Not equal counters");
      declare
         Test_Shared_R0 : SR.Shared_Reference;
      begin
         Test_Shared_R0.Init (TI.Encapsulated_Ref);
         declare
            Test_Shared_R1 : SR.Shared_Reference;
         begin
            Test_Shared_R.Init (Test_Shared_R0.Share);
            Test_Shared_R1.Init (Test_Shared_R0.Share);
            Result := 3;
            Ass.Assert ((Result = Test_Shared_R.Get_Counter), "Not equal counters");
            Ass.Assert ((Result = Test_Shared_R0.Get_Counter), "Not equal counters");
            Ass.Assert ((Result = Test_Shared_R1.Get_Counter), "Not equal counters");
         end;
         Result := 2;
         Ass.Assert ((Result = Test_Shared_R.Get_Counter), "Not equal counters");
         Ass.Assert ((Result = Test_Shared_R0.Get_Counter), "Not equal counters");
      end;
      Result := 1;
      Ass.Assert ((Result = Test_Shared_R.Get_Counter), "Not equal counters");
   end Test_Reference_Counting_3;

   -- Concurrent tests
   procedure Test_Concurrent_Share (TI : in out Shared_References_Test) is
      ST0 : Shared_Ref_Task;
      ST1 : Shared_Ref_Task;
      ST2 : Shared_Ref_Task;
   begin
      SR_Ref.Init (TI.Encapsulated_Ref);
      Ada.Text_IO.Put_Line ("Main::Counter [Concurrent] = " & Natural'Image (SR_Ref.Get_Counter));
      -- Invocation order
      Ada.Text_IO.Put_Line ("Invocation order :");
      Ada.Text_IO.Put_Line (Ada.Task_Identification.Image (ST0'Identity));
      Ada.Text_IO.Put_Line (Ada.Task_Identification.Image (ST1'Identity));
      Ada.Text_IO.Put_Line (Ada.Task_Identification.Image (ST2'Identity));
      -- Test concurrent access to the shared_reference (async calls)
      Ada.Text_IO.Put_Line ("Async execution :");
      ST0.Exec;
      ST1.Exec;
      ST2.Exec;
      -- Join tasks
      Ada.Text_IO.Put_Line ("Joining tasks...");
      ST0.Sync;
      ST1.Sync;
      ST2.Sync;
   end Test_Concurrent_Share;

   function Name(TI: Shared_References_Test) return AU.Message_String is
   begin
      return AU.Format ("Shared.References");
   end Name;

end Shared.Shared_References.Gen_Tests;