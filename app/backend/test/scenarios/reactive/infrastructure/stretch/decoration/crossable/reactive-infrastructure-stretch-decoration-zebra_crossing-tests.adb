with AUnit.Assertions;

with Active.Traveller.Mock;

with Reactive.District.Mock;

package body Reactive.Infrastructure.Stretch.Decoration.Zebra_Crossing.Tests is

   package Ass renames AUnit.Assertions;
   package Traveller_Pkg  renames Active.Traveller;
   package Traveller_Mock renames Active.Traveller.Mock;
   package District_Mock renames Reactive.District.Mock;

   Traveller : aliased Traveller_Mock.Object'Class
      := Traveller_Mock.Create.all;
   Traveller_Id : Agent.Agent_Id := Agent.Create_Id_From_Natural (800);

   -----------------------------------------------------
   --                   SCAFFOLDING
   -----------------------------------------------------
   procedure Set_Up (T : in out Zebra_Crossing_Test)
   is
      Traveller_Ref   : Traveller_Pkg.Reference := Traveller'Access;
      Traveller_Added : Boolean := False;
   begin
      Traveller.Set_Id (Traveller_Id);
      T.Stretch_Ref  := Stretch_Mock.Create;
      T.Trav_Queue := new Protected_Travellers_Queue;
      T.Trav_Queue.Set_Traveller_Utils (T.Trav_Utils);
      T.Trav_Queue.Set_Size (10);
      T.Trav_Utils := Traveller_Utils_Mock_Pkg.Create;
      T.Trav_Utils.Set_Return_Value_For_Get_Size (1);
      T.Stretch_Ref.Set_Return_Value_For_Get_Travellers_Queue (T.Trav_Queue);
      T.District_Ref := District.Reference (District_Mock.Create);
      T.District_Ref.Add_Traveller (Traveller_Ref, Traveller_Added);
      T.Privileged_Id := Agent.Create_Id_From_Natural (900);
      T.Unprivileged_Id := Agent.Create_Id_From_Natural (901);
   end Set_Up;

   procedure Tear_Down (T: in out Zebra_Crossing_Test) is
      Traveller_Removed : Boolean;
   begin
      T.District_Ref.Remove_Traveller (Traveller_Id, Traveller_Removed);
   end Tear_Down;

   procedure Tread (T            : in out Zebra_Crossing_Test;
                    Traveller_Id : in     Agent.Agent_Id;
                    Advanced     :    out Boolean) is
   begin
      T.Stretch_Ref.Set_Return_Value_For_Tread (True);
      T.Stretch_Ref.Set_Return_Value_For_Is_Waiting_To_Enter_Stretch (False);
      T.Crossing.Tread (Traveller_Id => Traveller_Id,
                        Advanced     => Advanced);
   end Tread;

   procedure Leave (T            : in out Zebra_Crossing_Test;
                    Traveller_Id : in     Agent.Agent_Id;
                    Left         :    out Boolean) is
   begin
      T.Stretch_Ref.Set_Return_Value_For_Leave (True);
      T.Crossing.Leave (Traveller_Id => Traveller_Id,
                        Left         => Left);
   end Leave;

   -----------------------------------------------------
   --                   SCENARIOS
   -----------------------------------------------------

   procedure Test_Tread_With_Free_Stretch (T : in out Zebra_Crossing_Test)
   is
      Advanced : Boolean := False;
   begin
      T.Tread (Traveller_Id, Advanced);
      Ass.Assert (Advanced, "The traveller did not tread the stretch");
   end Test_Tread_With_Free_Stretch;

   procedure Test_Tread_And_Then_Leave (T : in out Zebra_Crossing_Test)
   is
      Advanced : Boolean := False;
      Left     : Boolean := False;
   begin
      T.Tread (Traveller_Id, Advanced);
      Ass.Assert (Advanced, "The traveller did not tread the stretch");
      T.Leave (Traveller_Id, Left);
      Ass.Assert (Left, "The traveller did not leave the stretch");
   end Test_Tread_And_Then_Leave;

   procedure Test_Privileged_Waits_For_Unprivileged (
      T : in out Zebra_Crossing_Test)
   is
      Added    : Boolean;
      Removed  : Boolean;
      Advanced : Boolean := False;
   begin
      T.District_Ref.Add_Traveller (T.Privileged, Added);
      T.District_Ref.Add_Traveller (T.Unprivileged, Added);

      T.Tread (T.Unprivileged_Id, Advanced);
      Ass.Assert (Advanced, "The unprivileged did not tread the stretch");
      T.Tread (T.Privileged_Id, Advanced);
      Ass.Assert (not Advanced, "The privileged trod the stretch");

      Ass.Assert (
         T.Crossing.Priority_Enforcement.Has_Waiting_Privileged,
         "The privileged traveller was not enqueued");

      Ass.Assert (
         not T.Crossing.Priority_Enforcement.Has_Waiting_Unprivileged,
         "The unprivileged traveller is still enqueued");

      Ass.Assert (
         not T.Crossing.Priority_Enforcement.Has_Treading_Privileged,
         "The privileged traveller is treading the stretch");

      Ass.Assert (
         T.Crossing.Priority_Enforcement.Has_Treading_Unprivileged,
         "The unprivileged traveller is not treading the stretch");

      T.District_Ref.Remove_Traveller (T.Privileged_Id, Removed);
      T.District_Ref.Remove_Traveller (T.Unprivileged_Id, Removed);
   end Test_Privileged_Waits_For_Unprivileged;

   procedure Test_Unprivileged_Does_Not_Wait_For_Privileged (
      T : in out Zebra_Crossing_Test)
   is
      Added : Boolean;
      Removed : Boolean;
      Advanced : Boolean := False;
   begin
      T.District_Ref.Add_Traveller (T.Privileged, Added);
      T.District_Ref.Add_Traveller (T.Unprivileged, Added);

      T.Tread (T.Privileged_Id, Advanced);
      Ass.Assert (Advanced, "The privileged did not tread the stretch");
      T.Tread (T.Unprivileged_Id, Advanced);
      Ass.Assert (not Advanced, "The unprivileged trod the stretch");

      Ass.Assert (
         not T.Crossing.Priority_Enforcement.Has_Waiting_Unprivileged,
         "The unprivileged traveller was enqueued");

      Ass.Assert (
         not T.Crossing.Priority_Enforcement.Has_Waiting_Privileged,
         "The privileged traveller is still enqueued");

      Ass.Assert (
         T.Crossing.Priority_Enforcement.Has_Treading_Privileged,
         "The privileged traveller is not treading the stretch");

      Ass.Assert (
         not T.Crossing.Priority_Enforcement.Has_Treading_Unprivileged,
         "The unprivileged traveller is treading the stretch");

      T.District_Ref.Remove_Traveller (T.Privileged_Id, Removed);
      T.District_Ref.Remove_Traveller (T.Unprivileged_Id, Removed);
   end Test_Unprivileged_Does_Not_Wait_For_Privileged;

   procedure Test_Privileged_Treads_And_Leaves (
      T : in out Zebra_Crossing_Test)
   is
      Added    : Boolean;
      Removed  : Boolean;
      Advanced : Boolean := False;
      Left     : Boolean := False;
   begin
      T.District_Ref.Add_Traveller (T.Privileged, Added);

      T.Tread (T.Privileged_Id, Advanced);
      Ass.Assert (Advanced, "The privileged did not tread the stretch");

      Ass.Assert (
         not T.Crossing.Priority_Enforcement.Has_Waiting_Unprivileged,
         "An unprivileged traveller was enqueued");

      Ass.Assert (
         not T.Crossing.Priority_Enforcement.Has_Waiting_Privileged,
         "The privileged traveller is still enqueued");

      Ass.Assert (
         T.Crossing.Priority_Enforcement.Has_Treading_Privileged,
         "The privileged traveller is not treading the stretch");

      Ass.Assert (
         not T.Crossing.Priority_Enforcement.Has_Treading_Unprivileged,
         "An unprivileged traveller is treading the stretch");

      T.Leave (T.Privileged_Id, Left);
      Ass.Assert (Left, "The privileged did not leave the stretch");

      Ass.Assert (
         not T.Crossing.Priority_Enforcement.Has_Waiting_Unprivileged,
         "An unprivileged traveller was enqueued");

      Ass.Assert (
         not T.Crossing.Priority_Enforcement.Has_Waiting_Privileged,
         "The privileged traveller is still enqueued");

      Ass.Assert (
         not T.Crossing.Priority_Enforcement.Has_Treading_Privileged,
         "The privileged traveller is still treading the stretch");

      Ass.Assert (
         not T.Crossing.Priority_Enforcement.Has_Treading_Unprivileged,
         "An unprivileged traveller is treading the stretch");

      T.District_Ref.Remove_Traveller (T.Privileged_Id, Removed);
   end Test_Privileged_Treads_And_Leaves;

   procedure Test_Unprivileged_Treads_And_Leaves (
      T : in out Zebra_Crossing_Test)
   is
      Added    : Boolean;
      Removed  : Boolean;
      Advanced : Boolean := False;
      Left     : Boolean := False;
   begin
      T.District_Ref.Add_Traveller (T.Unprivileged, Added);

      T.Tread (T.Unprivileged_Id, Advanced);
      Ass.Assert (Advanced, "The unprivileged did not tread the stretch");

      Ass.Assert (
         not T.Crossing.Priority_Enforcement.Has_Waiting_Privileged,
         "A privileged traveller was enqueued");

      Ass.Assert (
         not T.Crossing.Priority_Enforcement.Has_Waiting_Unprivileged,
         "The Unprivileged traveller is still enqueued");

      Ass.Assert (
         T.Crossing.Priority_Enforcement.Has_Treading_Unprivileged,
         "The unprivileged traveller is not treading the stretch");

      Ass.Assert (
         not T.Crossing.Priority_Enforcement.Has_Treading_Privileged,
         "A privileged traveller is treading the stretch");

      T.Leave (T.Unprivileged_Id, Left);
      Ass.Assert (Left, "The Unprivileged did not leave the stretch");

      Ass.Assert (
         not T.Crossing.Priority_Enforcement.Has_Waiting_Privileged,
         "A privileged traveller was enqueued");

      Ass.Assert (
         not T.Crossing.Priority_Enforcement.Has_Waiting_Unprivileged,
         "The Unprivileged traveller is still enqueued");

      Ass.Assert (
         not T.Crossing.Priority_Enforcement.Has_Treading_Unprivileged,
         "The unprivileged traveller is still treading the stretch");

      Ass.Assert (
         not T.Crossing.Priority_Enforcement.Has_Treading_Privileged,
         "A privileged traveller is treading the stretch");

      T.District_Ref.Remove_Traveller (T.Unprivileged_Id, Removed);
   end Test_Unprivileged_Treads_And_Leaves;

   procedure Test_Privileged_Have_Right_Of_Way (
      T : in out Zebra_Crossing_Test)
   is
      Added    : Boolean;
      Removed  : Boolean;
      Advanced : Boolean := False;
      Left     : Boolean := False;
   begin
      T.District_Ref.Add_Traveller (T.Unprivileged, Added);
      T.District_Ref.Add_Traveller (T.Privileged, Added);

      T.Tread (T.Unprivileged_Id, Advanced);
      Ass.Assert (Advanced, "The unprivileged did not tread the stretch");

      Ass.Assert (
         not T.Crossing.Priority_Enforcement.Has_Waiting_Privileged,
         "A privileged traveller is currently enqueued");

      T.Tread (T.Privileged_Id, Advanced);
      Ass.Assert (not Advanced, "The privileged trod the stretch");

      Ass.Assert (
         T.Crossing.Priority_Enforcement.Has_Waiting_Privileged,
         "The privileged traveller was not enqueued");

      T.Leave (T.Unprivileged_Id, Left);
      Ass.Assert (Left, "The Unprivileged did not leave the stretch");

      T.Tread (T.Unprivileged_Id, Advanced);
      Ass.Assert (not Advanced, "The unprivileged trod the stretch with a"
                                & " privileged waiting for it");

      T.Tread (T.Privileged_Id, Advanced);
      Ass.Assert (Advanced, "The privileged was not able to tread the stretch"
                                & " after it was freed");

      T.District_Ref.Remove_Traveller (T.Unprivileged_Id, Removed);
      T.District_Ref.Remove_Traveller (T.Privileged_Id, Removed);
   end Test_Privileged_Have_Right_Of_Way;

   procedure Test_Unprivileged_Do_Not_Have_Right_Of_Way (
      T : in out Zebra_Crossing_Test)
   is
      Added    : Boolean;
      Removed  : Boolean;
      Advanced : Boolean := False;
      Left     : Boolean := False;
   begin
      T.District_Ref.Add_Traveller (T.Unprivileged, Added);
      T.District_Ref.Add_Traveller (T.Privileged, Added);

      T.Tread (T.Privileged_Id, Advanced);
      Ass.Assert (Advanced, "The unprivileged did not tread the stretch");

      Ass.Assert (
         not T.Crossing.Priority_Enforcement.Has_Waiting_Unprivileged,
         "An unprivileged traveller is currently enqueued");

      T.Tread (T.Unprivileged_Id, Advanced);
      Ass.Assert (not Advanced, "The privileged trod the stretch");

      Ass.Assert (
         not T.Crossing.Priority_Enforcement.Has_Waiting_Unprivileged,
         "The unprivileged traveller was enqueued");

      T.Leave (T.Privileged_Id, Left);
      Ass.Assert (Left, "The privileged did not leave the stretch");

      T.Tread (T.Privileged_Id, Advanced);
      Ass.Assert (
         Advanced,
         "The privileged was not able to tread the stretch with an"
         & " unprivileged waiting for it");

      T.Tread (T.Unprivileged_Id, Advanced);
      Ass.Assert (
         not Advanced,
         "The unprivileged trod the stretch after an attempt of a privileged"
         & "  to tread it");

      T.District_Ref.Remove_Traveller (T.Unprivileged_Id, Removed);
      T.District_Ref.Remove_Traveller (T.Privileged_Id, Removed);
   end Test_Unprivileged_Do_Not_Have_Right_Of_Way;

   -----------------------------------------------------
   --                 TEST WRAPPERS
   -----------------------------------------------------

   procedure Test_Tread_With_Free_Stretch_Wrapper (
      T : in out Zebra_Crossing_Test'Class)
   is
   begin
      Test_Tread_With_Free_Stretch (T);
   end Test_Tread_With_Free_Stretch_Wrapper;

   procedure Test_Tread_And_Then_Leave_Wrapper (
      T : in out Zebra_Crossing_Test'Class)
   is
   begin
      Test_Tread_And_Then_Leave (T);
   end Test_Tread_And_Then_Leave_Wrapper;

   procedure Test_Privileged_Waits_For_Unprivileged_Wrapper (
      T : in out Zebra_Crossing_Test'Class)
   is
   begin
      Test_Privileged_Waits_For_Unprivileged (T);
   end Test_Privileged_Waits_For_Unprivileged_Wrapper;

   procedure Test_Unprivileged_Does_Not_Wait_For_Privileged_Wrapper (
      T : in out Zebra_Crossing_Test'Class)
   is
   begin
      Test_Unprivileged_Does_Not_Wait_For_Privileged (T);
   end Test_Unprivileged_Does_Not_Wait_For_Privileged_Wrapper;

   procedure Test_Privileged_Treads_And_Leaves_Wrapper (
      T : in out Zebra_Crossing_Test'Class)
   is
   begin
      Test_Privileged_Treads_And_Leaves (T);
   end Test_Privileged_Treads_And_Leaves_Wrapper;

   procedure Test_Unprivileged_Treads_And_Leaves_Wrapper (
      T : in out Zebra_Crossing_Test'Class)
   is
   begin
      Test_Unprivileged_Treads_And_Leaves (T);
   end Test_Unprivileged_Treads_And_Leaves_Wrapper;

   procedure Test_Privileged_Have_Right_Of_Way_Wrapper (
      T : in out Zebra_Crossing_Test'Class)
   is
   begin
      Test_Privileged_Have_Right_Of_Way (T);
   end Test_Privileged_Have_Right_Of_Way_Wrapper;

   procedure Test_Unprivileged_Do_Not_Have_Right_Of_Way_Wrapper (
      T : in out Zebra_Crossing_Test'Class)
   is
   begin
      Test_Unprivileged_Do_Not_Have_Right_Of_Way (T);
   end Test_Unprivileged_Do_Not_Have_Right_Of_Way_Wrapper;

    -----------------------------------------------------
   --                  REGISTRATION
   -----------------------------------------------------
   procedure Register_Tests (T: in out Zebra_Crossing_Test) is
      package Register_Specific is
         new TC.Specific_Test_Case_Registration (Zebra_Crossing_Test);
      use Register_Specific;
   begin

      Register_Wrapper (
         Test    => T,
         Routine => Test_Tread_With_Free_Stretch_Wrapper'Access,
         Name    => "Tests a traveller treading a free crossing stretch");

      Register_Wrapper (
         Test    => T,
         Routine => Test_Tread_And_Then_Leave_Wrapper'Access,
         Name    => "Tests a traveller leaves a stretch after treading it");

      Register_Wrapper (
         Test    => T,
         Routine => Test_Privileged_Waits_For_Unprivileged_Wrapper'Access,
         Name    => "Tests a privileged traveller waits for an unprivileged to"
                    & " free the stretch");

      Register_Wrapper (
         Test    => T,
         Routine =>
            Test_Unprivileged_Does_Not_Wait_For_Privileged_Wrapper'Access,
         Name    => "Tests an unprivileged traveller does not waits for a"
                    & " privileged to free the stretch");

      Register_Wrapper (
         Test    => T,
         Routine => Test_Privileged_Treads_And_Leaves_Wrapper'Access,
         Name    => "Tests a privileged traveller treads and leaves neatly");

      Register_Wrapper (
         Test    => T,
         Routine => Test_Unprivileged_Treads_And_Leaves_Wrapper'Access,
         Name    =>
            "Tests an unprivileged traveller treads and leaves neatly");

      Register_Wrapper (
         Test    => T,
         Routine => Test_Privileged_Have_Right_Of_Way_Wrapper'Access,
         Name    => "Tests a privileged traveller has the right of way");

      Register_Wrapper (
         Test    => T,
         Routine => Test_Unprivileged_Do_Not_Have_Right_Of_Way_Wrapper'Access,
         Name    =>
            "Tests an unprivileged traveller does not have the right of way");

   end Register_Tests;

end Reactive.Infrastructure.Stretch.Decoration.Zebra_Crossing.Tests;
