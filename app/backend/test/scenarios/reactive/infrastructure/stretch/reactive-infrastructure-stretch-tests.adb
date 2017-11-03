with AUnit.Assertions;

package body Reactive.Infrastructure.Stretch.Tests is
   package Ass renames AUnit.Assertions;

   -----------------------------------------------------
   --                  SCAFFOLDING
   -----------------------------------------------------

   procedure Tear_Down (T: in out Stretch_Test) is
   begin
      T.Stretch.Protected_Travellers_Queue.Clear;
   end Tear_Down;

   procedure Tread (T            : in out Stretch_Test;
                    Traveller_Id : in     Agent.Agent_Id;
                    Advanced     :    out Boolean)
   is
      Entered : Boolean := TRUE;
   begin
      T.Stretch.Tread (Traveller_Id => Traveller_Id,
                       Advanced     => Advanced);
   end Tread;

   procedure Leave (T            : in out Stretch_Test;
                    Traveller_Id : in     Agent.Agent_Id;
                    Left         :    out Boolean)
   is
      Entered : Boolean := False;
   begin
      T.Stretch.Tread (
         Traveller_Id => Traveller_Id, Advanced => Entered);
      Ass.Assert (Entered,
                  "The traveller is not entered into the stretch");
      T.Stretch.Leave (Traveller_Id => Traveller_Id,
                       Left         => Left);
   end Leave;

   procedure Is_Before (
      T               : in out Stretch_Test;
      Other_Stretch   : in     Stretch.Object'Class;
      Stretches_Count : in     Natural;
      Before          :    out Boolean)
   is
      Stretch_Position : Natural := 0;
   begin
      T.Lane_Utils.Set_Return_Value_For_Count_Stretches (Stretches_Count);
      T.Lane_Utils.Set_Return_Value_For_Find_Stretch_Position (
         Stretch_Id => T.Stretch.Id,
         Return_Value => Stretch_Position,
         Found => TRUE);
      T.Lane_Utils.Set_Return_Value_For_Get_Direction (
         Lane_Id   => T.Stretch.Lane_Id,
         Direction => Shared.Direction.SOUTH_NORTH);
      Before := T.Stretch.Is_Before (Other_Stretch);
   end Is_Before;

   procedure Fill_Stretch (T: in out Stretch_Test)
   is
      Traveller1_Id : Agent.Agent_Id := Agent.Create_Id_From_Natural (85);
      Traveller2_Id : Agent.Agent_Id := Agent.Create_Id_From_Natural (86);
      Entered       : Boolean := FALSE;
   begin
      T.Stretch.Size := 2;
      T.Stretch.Tread (
         Traveller_Id => Traveller1_Id, Advanced => Entered);

      Ass.Assert (Entered,
                  "The traveller is not entered into the Stretch");

      Entered := FALSE;

      T.Stretch.Tread (
         Traveller_Id => Traveller2_Id, Advanced => Entered);

      Ass.Assert (Entered,
                  "The traveller is not entered into the Stretch");
   end Fill_Stretch;

   -----------------------------------------------------
   --                   SCENARIOS
   -----------------------------------------------------

   procedure Test_Tread_When_Stretch_Not_Full (T: in out Stretch_Test)
   is
      Advanced : Boolean := FALSE;
      Left     : Boolean := FALSE;
      Traveller_Id : Agent.Agent_Id := Agent.Create_Id_From_Natural (84);
   begin
      T.Tread (Traveller_Id => Traveller_Id,
               Advanced     => Advanced);

      Ass.Assert (Advanced,
                  "The traveller is not advanced into the Stretch");
   end Test_Tread_When_Stretch_Not_Full;

   procedure Test_Tread_When_Stretch_Full (T: in out Stretch_Test)
   is
      Advanced : Boolean := FALSE;
      Traveller_Id : Agent.Agent_Id := Agent.Create_Id_From_Natural (87);
   begin
      T.Fill_Stretch;

      T.Tread (Traveller_Id => Traveller_Id,
               Advanced     => Advanced);

      Ass.Assert ((Advanced),
                  "The traveller is not advanced into the Stretch");
   end Test_Tread_When_Stretch_Full;

   procedure Test_No_Tread_When_Already_Inside (T: in out Stretch_Test)
   is
      Advanced, Entered : Boolean := FALSE;
      Traveller_Id : Agent.Agent_Id := Agent.Create_Id_From_Natural (88);
   begin
      T.Stretch.Tread (Traveller_Id, Entered);

      Ass.Assert (Entered,
                  "The traveller is not entered into waiting list");


      T.Stretch.Tread (Traveller_Id => Traveller_Id,
                       Advanced     => Advanced);

      Ass.Assert (Advanced,
                  "The traveller is advanced into the Stretch");

   end Test_No_Tread_When_Already_Inside;

   procedure Test_Tread_After_Exit_From_Waiting_List (T: in out Stretch_Test)
   is
      Advanced, Entered : Boolean := FALSE;
      Traveller_Id : Agent.Agent_Id := Agent.Create_Id_From_Natural (84);
   begin
      T.Stretch.Protected_Travellers_Queue.Enter_Into_Waiting_List (Traveller_Id, Entered);

      Ass.Assert ((Entered),
                  "The traveller is not entered into waiting list");


      T.Stretch.Tread (Traveller_Id => Traveller_Id,
                       Advanced     => Advanced);

      Ass.Assert ((Advanced),
                  "The traveller is not advanced into the Stretch");

   end Test_Tread_After_Exit_From_Waiting_List;

   procedure Test_Leave_Stretch (T: in out Stretch_Test)
   is
      Traveller_Id : Agent.Agent_Id := Agent.Create_Id_From_Natural (23);
      Left : Boolean;
   begin
      T.Leave (Traveller_Id => Traveller_Id,
               Left         => Left);

      Ass.Assert ((Left),
                  "The traveller is not entered into the Stretch");
   end Test_Leave_Stretch;

   procedure Test_Find_Street (T: in out Stretch_Test)
   is
      Street_Id : Infra_Id := 47;
   begin
      T.Lane_Utils.Set_Return_Value_For_Find_Street (Street_Id);

      Ass.Assert ((T.Stretch.Find_Street = Street_Id),
                  "The id returned by Street's finder is wrong");
   end Test_Find_Street;

   procedure Test_Intersections_Finder (T: in out Stretch_Test)
   is
      Intersection_Id : Infra_Id := 256;
      Intersections : Infra_Id_Set.Set;
   begin
      Intersections.Insert (New_Item => Intersection_Id);

      T.Lane_Utils.Set_Return_Value_For_Find_Intersections (
         Return_Value => Intersections);

      Ass.Assert (
         (Natural(T.Stretch.Find_Intersections.Length) = 1),
        "The intersections found are different from specified ones.");

      Ass.Assert (
         (not T.Stretch.Find_Intersections.Is_Empty
          and T.Stretch.Find_Intersections.First_Element = Intersection_Id
         ),
         "The intersections found are different from specified ones.");
   end Test_Intersections_Finder;

   procedure Test_Find_Lane (T : in out Stretch_Test) is
   begin
      Ass.Assert (
         T.Stretch.Find_Lane = T.Stretch.Lane_Id,
         "The id returned by Street's finder is different by the one set"
            & " as stretch container");
   end Test_Find_Lane;

   procedure Test_Stretch_Id_Getter (T: in out Stretch_Test) is
   begin
      Ass.Assert ((T.Stretch.Get_Id = T.Stretch.Id),
                  "The id returned by Stretch's getter is wrong");
   end Test_Stretch_Id_Getter;

   procedure Test_Lane_Setter (T: in out Stretch_Test)
   is
      Lane_Id : Infra_Id := 45;
   begin
      T.Stretch.Set_Lane (Lane_Id);

      Ass.Assert ((T.Stretch.Lane_Id = Lane_Id),
                  "The lane id is not correctly set");
   end Test_Lane_Setter;

   procedure Test_Is_Before_Into_The_Same_Lane (T : in out Stretch_Test)
   is
      Stretch2          : Stretch.Object'Class := T.Stretch.all;
      Stretch2_Id       : Infra_Id := 34;
      Stretch2_Size     : Natural := 45;
      Before            : Boolean := FALSE;
      Stretches_Count   : Natural := 2;
      Stretch1_Position : Natural := 2;
      Stretch2_Position : Natural := 4;
   begin
      Stretch2.Id := Stretch2_Id;
      Stretch2.Size := Stretch2_Size;
      Stretch2.Lane_Id := T.Stretch.Lane_Id;

      T.Lane_Utils.Set_Return_Value_For_Count_Stretches (Stretches_Count);
      T.Lane_Utils.Set_Return_Value_For_Find_Stretch_Position (
         Stretch_Id => T.Stretch.Id,
         Return_Value => Stretch1_Position,
         Found => TRUE);
      T.Lane_Utils.Set_Return_Value_For_Find_Stretch_Position (
         Stretch_Id => Stretch2_Id,
         Return_Value => Stretch2_Position,
         Found => TRUE);
      T.Lane_Utils.Set_Return_Value_For_Get_Direction (
         Lane_Id   => T.Stretch.Lane_Id,
         Direction => Shared.Direction.SOUTH_NORTH);

      Before := T.Stretch.Is_Before (Stretch2);
      Ass.Assert (Before, "the stretch is not before");
   end Test_Is_Before_Into_The_Same_Lane;

   procedure Test_Is_After_Into_The_Same_Lane (T : in out Stretch_Test)
   is
      Stretch2          : Stretch.Object'Class := T.Stretch.all;
      Stretch2_Id       : Infra_Id := 34;
      Stretch2_Size     : Natural := 45;
      Before            : Boolean := FALSE;
      Stretches_Count   : Natural := 2;
      Stretch1_Position : Natural := 4;
      Stretch2_Position : Natural := 2;
   begin
      Stretch2.Id := Stretch2_Id;
      Stretch2.Size := Stretch2_Size;
      Stretch2.Lane_Id := T.Stretch.Lane_Id;

      T.Lane_Utils.Set_Return_Value_For_Count_Stretches (Stretches_Count);
      T.Lane_Utils.Set_Return_Value_For_Find_Stretch_Position (
         Stretch_Id => T.Stretch.Id,
         Return_Value => Stretch1_Position,
         Found => TRUE);
      T.Lane_Utils.Set_Return_Value_For_Find_Stretch_Position (
         Stretch_Id => Stretch2_Id,
         Return_Value => Stretch2_Position,
         Found => TRUE);
      T.Lane_Utils.Set_Return_Value_For_Get_Direction (
         Lane_Id   => T.Stretch.Lane_Id,
         Direction => Shared.Direction.SOUTH_NORTH);

      Before := T.Stretch.Is_Before (Stretch2);
      Ass.Assert (not Before, "the stretch is before");
   end Test_Is_After_Into_The_Same_Lane;

   procedure Test_Is_Before_Not_Into_The_Same_Lane (T : in out Stretch_Test)
   is
      Stretch2          : Stretch.Object'Class := T.Stretch.all;
      Stretch2_Id       : Infra_Id := 34;
      Before            : Boolean := FALSE;
      Stretches_Count   : Natural := 2;
      Stretch1_Position : Natural := 1;
      Stretch2_Position : Natural;
   begin
      Stretch2.Id := Stretch2_Id;
      Stretch2.Size := T.Stretch.Size;
      Stretch2.Lane_Id := T.Stretch.Lane_Id + 1;
      Stretch2_Position := Stretch2.Size - Stretch1_Position - 3;

      T.Lane_Utils.Set_Return_Value_For_Count_Stretches (Stretches_Count);
      T.Lane_Utils.Set_Return_Value_For_Find_Stretch_Position (
         Stretch_Id => T.Stretch.Id,
         Return_Value => Stretch1_Position,
         Found => TRUE);
      T.Lane_Utils.Set_Return_Value_For_Find_Stretch_Position (
         Stretch_Id => Stretch2_Id,
         Return_Value => Stretch2_Position,
         Found => TRUE);
      T.Lane_Utils.Set_Return_Value_For_Get_Direction (
         Lane_Id   => T.Stretch.Lane_Id,
         Direction => Shared.Direction.SOUTH_NORTH);
      T.Lane_Utils.Set_Return_Value_For_Get_Direction (
         Lane_Id   => Stretch2.Lane_Id,
         Direction => Shared.Direction.NORTH_SOUTH);

      Before := T.Stretch.Is_Before (Stretch2);
      Ass.Assert (Before, "the stretch is not before");
   end Test_Is_Before_Not_Into_The_Same_Lane;

   procedure Test_Is_Not_Before_Not_Into_The_Same_Lane (
      T : in out Stretch_Test)
   is
      Stretch2          : Stretch.Object'Class := T.Stretch.all;
      Stretch2_Id       : Infra_Id := 34;
      Before            : Boolean := FALSE;
      Stretches_Count   : Natural := 2;
      Stretch1_Position : Natural := 1;
      Stretch2_Position : Natural;
   begin
      Stretch2.Id := Stretch2_Id;
      Stretch2.Size := T.Stretch.Size;
      Stretch2.Lane_Id := T.Stretch.Lane_Id + 1;
      Stretch2_Position := Stretch2.Size - Stretch1_Position - 1;

      T.Lane_Utils.Set_Return_Value_For_Count_Stretches (Stretches_Count);
      T.Lane_Utils.Set_Return_Value_For_Find_Stretch_Position (
         Stretch_Id   => T.Stretch.Id,
         Return_Value => Stretch1_Position,
         Found        => TRUE);
      T.Lane_Utils.Set_Return_Value_For_Find_Stretch_Position (
         Stretch_Id   => Stretch2_Id,
         Return_Value => Stretch2_Position,
         Found        => TRUE);
      T.Lane_Utils.Set_Return_Value_For_Get_Direction (
         Lane_Id   => T.Stretch.Lane_Id,
         Direction => Shared.Direction.SOUTH_NORTH);
      T.Lane_Utils.Set_Return_Value_For_Get_Direction (
         Lane_Id   => Stretch2.Lane_Id,
         Direction => Shared.Direction.NORTH_SOUTH);

      Before := T.Stretch.Is_Before (Stretch2);
      Ass.Assert (not Before, "the stretch is before");
   end Test_Is_Not_Before_Not_Into_The_Same_Lane;

   procedure Test_A_Stretch_Is_Not_Before_Itself (T : in out Stretch_Test)
   is
      Before : Boolean := TRUE;
   begin
      T.Is_Before (Other_Stretch   => T.Stretch.all,
                   Stretches_Count => 1,
                   Before          => Before);
      Ass.Assert (not Before, "the stretch is before itself");
   end Test_A_Stretch_Is_Not_Before_Itself;

   procedure Test_Is_Waiting_To_Enter_Stretch (T : in out Stretch_Test)
   is
      Traveller_Id : Agent.Agent_Id := Agent.Create_Id_From_Natural (34);
      Entered : Boolean;
   begin
      T.Stretch.Protected_Travellers_Queue.Enter_Into_Waiting_List (
         Traveller_Id => Traveller_Id, Entered => Entered);

      Ass.Assert (Entered,
                  "The traveller is not entered into waiting list");

      Ass.Assert (
         T.Stretch.Is_Waiting_To_Enter_Stretch (Traveller_Id => Traveller_Id),
         "The traveller is not waiting to enter stretch");
   end Test_Is_Waiting_To_Enter_Stretch;

   procedure Test_Is_Not_Waiting_To_Enter_Stretch (T : in out Stretch_Test)
     is
      Traveller_Id : Agent.Agent_Id := Agent.Create_Id_From_Natural (345);
   begin
      Ass.Assert (
         not T.Stretch.Is_Waiting_To_Enter_Stretch (
            Traveller_Id => Traveller_Id),
         "The traveller is waiting to enter stretch");
   end Test_Is_Not_Waiting_To_Enter_Stretch;

   procedure Test_Host_Setter (T : in out Stretch_Test)
   is
      Host_Id : Infra_Id := 42;
   begin
      T.Stretch.Set_Host (Host_Id);
      Ass.Assert(
         T.Stretch.Host_Id = Host_Id,
         "Host setter did not set the host id");
   end Test_Host_Setter;

   procedure Test_Host_Getter (T : in out Stretch_Test)
   is
      Host_Id : Infra_Id := 44;
   begin
      T.Stretch.Host_Id := Host_Id;
      Ass.Assert(
         T.Stretch.Get_Host = Host_Id,
         "Host getter did not return the host id");
   end Test_Host_Getter;

   procedure Test_Has_Host (T : in out Stretch_Test)
   is
      Host_Id : Infra_Id := 44;
   begin
      Ass.Assert(
         not T.Stretch.Has_Host,
         "Boolean check for host returns True with host not set");
      T.Stretch.Set_Host (Host_Id);
      Ass.Assert(
         T.Stretch.Has_Host,
         "Boolean check for host returns False with host set");
   end Test_Has_Host;

   ------------------------------
   ---        WRAPPERS
   ------------------------------

   procedure Test_Tread_When_Stretch_Not_Full_Wrapper (
      T : in out Stretch_Test'Class) is
   begin
      Test_Tread_When_Stretch_Not_Full (T);
   end Test_Tread_When_Stretch_Not_Full_Wrapper;

   procedure Test_Tread_When_Stretch_Full_Wrapper (
      T : in out Stretch_Test'Class) is
   begin
      Test_Tread_When_Stretch_Full (T);
   end Test_Tread_When_Stretch_Full_Wrapper;

   procedure Test_No_Tread_When_Already_Inside_Wrapper (
      T : in out Stretch_Test'Class) is
   begin
      Test_No_Tread_When_Already_Inside (T);
   end Test_No_Tread_When_Already_Inside_Wrapper;

   procedure Test_Tread_After_Exit_From_Waiting_List_Wrapper (
      T : in out Stretch_Test'Class) is
   begin
      Test_Tread_After_Exit_From_Waiting_List (T);
   end Test_Tread_After_Exit_From_Waiting_List_Wrapper;

   procedure Test_Leave_Stretch_Wrapper (T : in out Stretch_Test'Class) is
   begin
      Test_Leave_Stretch (T);
   end Test_Leave_Stretch_Wrapper;

   procedure Test_Find_Street_Wrapper (T : in out Stretch_Test'Class) is
   begin
      Test_Find_Street (T);
   end Test_Find_Street_Wrapper;

   procedure Test_Intersections_Finder_Wrapper (
      T : in out Stretch_Test'Class) is
   begin
      Test_Intersections_Finder (T);
   end Test_Intersections_Finder_Wrapper;

   procedure Test_Find_Lane_Wrapper (T : in out Stretch_Test'Class) is
   begin
      Test_Find_Lane (T);
   end Test_Find_Lane_Wrapper;

   procedure Test_Stretch_Id_Getter_Wrapper (T : in out Stretch_Test'Class) is
   begin
      Test_Stretch_Id_Getter (T);
   end Test_Stretch_Id_Getter_Wrapper;

   procedure Test_Lane_Setter_Wrapper (T : in out Stretch_Test'Class) is
   begin
      Test_Lane_Setter (T);
   end Test_Lane_Setter_Wrapper;

   procedure Test_Is_Before_Into_The_Same_Lane_Wrapper (
      T : in out Stretch_Test'Class) is
   begin
      Test_Is_Before_Into_The_Same_Lane (T);
   end Test_Is_Before_Into_The_Same_Lane_Wrapper;

   procedure Test_Is_After_Into_The_Same_Lane_Wrapper (
      T : in out Stretch_Test'Class) is
   begin
      Test_Is_Before_Into_The_Same_Lane (T);
   end Test_Is_After_Into_The_Same_Lane_Wrapper;

   procedure Test_Is_Before_Not_Into_The_Same_Lane_Wrapper (
      T : in out Stretch_Test'Class) is
   begin
      Test_Is_Before_Not_Into_The_Same_Lane (T);
   end Test_Is_Before_Not_Into_The_Same_Lane_Wrapper;

   procedure  Test_Is_Not_Before_Not_Into_The_Same_Lane_Wrapper (
      T : in out Stretch_Test'Class) is
   begin
      Test_Is_Not_Before_Not_Into_The_Same_Lane (T);
   end Test_Is_Not_Before_Not_Into_The_Same_Lane_Wrapper;

   procedure Test_A_Stretch_Is_Not_Before_Itself_Wrapper (
      T : in out Stretch_Test'Class) is
   begin
      Test_A_Stretch_Is_Not_Before_Itself (T);
   end Test_A_Stretch_Is_Not_Before_Itself_Wrapper;

   procedure Test_Is_Waiting_To_Enter_Stretch_Wrapper (
      T : in out Stretch_Test'Class) is
   begin
      Test_Is_Waiting_To_Enter_Stretch (T);
   end Test_Is_Waiting_To_Enter_Stretch_Wrapper;

   procedure Test_Is_Not_Waiting_To_Enter_Stretch_Wrapper (
      T : in out Stretch_Test'Class) is
   begin
      Test_Is_Not_Waiting_To_Enter_Stretch (T);
   end Test_Is_Not_Waiting_To_Enter_Stretch_Wrapper;

   procedure Test_Host_Setter_Wrapper (
      T : in out Stretch_Test'Class) is
   begin
      Test_Host_Setter (T);
   end Test_Host_Setter_Wrapper;

   procedure Test_Host_Getter_Wrapper (
      T : in out Stretch_Test'Class) is
   begin
      Test_Host_Getter (T);
   end Test_Host_Getter_Wrapper;

   procedure Test_Has_Host_Wrapper (
      T : in out Stretch_Test'Class) is
   begin
      Test_Has_Host (T);
   end Test_Has_Host_Wrapper;

    -----------------------------------------------------
   --                  REGISTRATION
   -----------------------------------------------------
   procedure Register_Tests (T: in out Stretch_Test) is
      package Register_Specific is
         new TC.Specific_Test_Case_Registration (Stretch_Test);
      use Register_Specific;
   begin

      Register_Wrapper (
         Test    => T,
         Routine => Test_Tread_When_Stretch_Not_Full_Wrapper'Access,
         Name    => "Test stretch not full treading");

      Register_Wrapper (
         Test    => T,
         Routine => Test_Tread_When_Stretch_Full_Wrapper'Access,
         Name    => "Test stretch full treading");

      Register_Wrapper (
         Test    => T,
         Routine => Test_No_Tread_When_Already_Inside_Wrapper'Access,
         Name    => "Test stretch treading when already inside stretch");

      Register_Wrapper (
         Test    => T,
         Routine => Test_Tread_After_Exit_From_Waiting_List_Wrapper'Access,
         Name    => "Test stretch treading after exit from waiting list");

      Register_Wrapper (
         Test    => T,
         Routine => Test_Leave_Stretch_Wrapper'Access,
         Name    => "Test a traveller leaving a stretch");

      Register_Wrapper (
         Test    => T,
         Routine => Test_Find_Street_Wrapper'Access,
         Name    =>
            "Test street's finder returns the id of the street associated"
               & " to the Stretch");

      Register_Wrapper (
         Test    => T,
         Routine => Test_Intersections_Finder_Wrapper'Access,
         Name    =>
            "Test intersections' finder returns the ids of the"
               & " intersections associated to the Stretch");

       Register_Wrapper (
         Test    => T,
         Routine => Test_Find_Lane_Wrapper'Access,
         Name    =>
            "Test lane's finder returns the id of the lane associated to"
               & "the Stretch");

      Register_Wrapper (
         Test    => T,
         Routine => Test_Stretch_Id_Getter_Wrapper'Access,
         Name    => "Test Stretch id getter");

      Register_Wrapper (
         Test    => T,
         Routine => Test_Lane_Setter_Wrapper'Access,
         Name    => "Test lane setter");

      Register_Wrapper (
         Test    => T,
         Routine => Test_A_Stretch_Is_Not_Before_Itself_Wrapper'Access,
         Name    => "Test the stretch is not before itself");

      Register_Wrapper (
         Test    => T,
         Routine => Test_Is_Before_Into_The_Same_Lane_Wrapper'Access,
         Name    => "Test is before into the same lane");

      Register_Wrapper (
         Test    => T,
         Routine => Test_Is_After_Into_The_Same_Lane_Wrapper'Access,
         Name    => "Test is after into the same lane");

      Register_Wrapper (
         Test    => T,
         Routine => Test_Is_Before_Not_Into_The_Same_Lane_Wrapper'Access,
         Name    => "Test is before not into the same lane");

      Register_Wrapper (
         Test    => T,
         Routine => Test_Is_Not_Before_Not_Into_The_Same_Lane_Wrapper'Access,
         Name    => "Test is not before not into the same lane");

      Register_Wrapper (
         Test    => T,
         Routine => Test_Is_Waiting_To_Enter_Stretch_Wrapper'Access,
         Name    => "Test a traveller is waiting to enter stretch");

      Register_Wrapper (
         Test    => T,
         Routine => Test_Is_Not_Waiting_To_Enter_Stretch_Wrapper'Access,
         Name    => "Test a traveller is not waiting to enter stretch");

      Register_Wrapper (
         Test    => T,
         Routine => Test_Host_Setter_Wrapper'Access,
         Name    => "Test the setter of the host id");

      Register_Wrapper (
         Test    => T,
         Routine => Test_Host_Getter_Wrapper'Access,
         Name    => "Test the getter of the host id");

      Register_Wrapper (
         Test    => T,
         Routine => Test_Has_Host_Wrapper'Access,
         Name    => "Test the boolean check for the host id");
   end Register_Tests;
end Reactive.Infrastructure.Stretch.Tests;
