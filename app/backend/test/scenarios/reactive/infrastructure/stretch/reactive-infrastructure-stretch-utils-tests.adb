with AUnit.Assertions;
with Ada.Text_IO;

with Active.Agent;

with Reactive.District.Mock;
with Reactive.Infrastructure.Stretch.Mock;

package body Reactive.Infrastructure.Stretch.Utils.Tests is
   package Ass renames AUnit.Assertions;
   package Agent renames Active.Agent;
   package District_Mock renames Reactive.District.Mock;
   package Stretch_Mock renames Reactive.Infrastructure.Stretch.Mock;

   -----------------------------------------------------
   --                   SCAFFOLDING
   -----------------------------------------------------
   Stretch_Utils : Reactive.Infrastructure.Stretch.Utils.Reference;
   District : access District_Mock.Object;

   procedure Set_Up (T: in out Stretch_Utils_Test) is
   begin
      District := District_Mock.Create;

      Stretch_Utils
        := Reactive.Infrastructure.Stretch.Utils.Get_Instance (District => District);
   end Set_Up;

   procedure Test_Id_Getter (T: in out TC.Test_Case'Class)
   is
      Stretch : aliased Stretch_Mock.Reference := Stretch_Mock.Create;
      Stretch_Ref : aliased Reactive.Infrastructure.Stretch.Reference
         := Reactive.Infrastructure.Stretch.Reference (Stretch);
      Stretch_Id : Infra_Id := 345;
      Added : Boolean := FALSE;
   begin
      Stretch.Set_Id (Stretch_Id);
      District.Add_Stretch (Infrastructure => Stretch_Ref,
                            Added          => Added);

      Ass.Assert (Added,
                  "The stretch is not added to district");

      Ass.Assert (Stretch_Utils.Get_Id (Stretch_Id => Stretch_Id)
                  = Stretch_Id,
                  "The value returned by Stretch id getter is wrong");
   end Test_Id_Getter;

   procedure Test_Tread (T: in out TC.Test_Case'Class)
   is
      Stretch : aliased Stretch_Mock.Reference := Stretch_Mock.Create;
      Stretch_Ref : aliased Reactive.Infrastructure.Stretch.Reference
         := Reactive.Infrastructure.Stretch.Reference (Stretch);
      Stretch_Id : Infra_Id := 345;
      Vehicle_Id : Agent.Agent_Id := Agent.Create_Id_From_Natural (433);
      Added, Advanced : Boolean := FALSE;
   begin
      Stretch.Set_Id (Stretch_Id);
      District.Add_Stretch (Infrastructure => Stretch_Ref,
                            Added          => Added);

      Stretch.Set_Return_Value_For_Tread (TRUE);

      Stretch_Utils.Tread (Stretch_Id => Stretch_Id,
                           Traveller_Id => Vehicle_Id,
                           Advanced   => Advanced);

      Ass.Assert (Advanced,
                  "The roadway is not treading by the vehicle");
   end Test_Tread;

   procedure Test_Not_Tread (T: in out TC.Test_Case'Class)
   is
      Stretch : aliased Stretch_Mock.Reference := Stretch_Mock.Create;
      Stretch_Ref : aliased Reactive.Infrastructure.Stretch.Reference
         := Reactive.Infrastructure.Stretch.Reference (Stretch);
      Stretch_Id : Infra_Id := 345;
      Vehicle_Id : Agent.Agent_Id := Agent.Create_Id_From_Natural (434);
      Added, Advanced : Boolean := TRUE;
   begin
      Stretch.Set_Id (Stretch_Id);
      District.Add_Stretch (Infrastructure => Stretch_Ref,
                            Added          => Added);

      Stretch.Set_Return_Value_For_Tread (FALSE);

      Stretch_Utils.Tread (Stretch_Id => Stretch_Id,
                           Traveller_Id => Vehicle_Id,
                           Advanced   => Advanced);

      Ass.Assert (not Advanced,
                  "The roadway is treading by the vehicle");

   end Test_Not_Tread;

   procedure Test_Traveller_Is_Waiting_To_Enter_Stretch (T: in out TC.Test_Case'Class)
   is
      Stretch : aliased Stretch_Mock.Reference := Stretch_Mock.Create;
      Stretch_Ref : aliased Reactive.Infrastructure.Stretch.Reference
         := Reactive.Infrastructure.Stretch.Reference (Stretch);
      Stretch_Id : Infra_Id := 345;
      Traveller_Id : Agent.Agent_Id := Agent.Create_Id_From_Natural (435);
      Added : Boolean := FALSE;
   begin
      Stretch.Set_Id (Stretch_Id);
      District.Add_Stretch (Infrastructure => Stretch_Ref,
                            Added          => Added);
      Stretch.Set_Return_Value_For_Is_Waiting_To_Enter_Stretch (TRUE);

      Ass.Assert (Added,
                  "The stretch is not added to district");

      Ass.Assert (Stretch_Utils.Is_Waiting_To_Enter_Stretch
                  (Stretch_Id   => Stretch_Id,
                   Traveller_Id => Traveller_Id),
                  "The traveller is not waiting to enter stretch");
   end Test_Traveller_Is_Waiting_To_Enter_Stretch;

   procedure Test_Traveller_Is_Not_Waiting_To_Enter_Stretch (T: in out TC.Test_Case'Class)
   is
      Stretch : aliased Stretch_Mock.Reference := Stretch_Mock.Create;
      Stretch_Ref : aliased Reactive.Infrastructure.Stretch.Reference
         := Reactive.Infrastructure.Stretch.Reference (Stretch);
      Stretch_Id : Infra_Id := 345;
      Traveller_Id : Agent.Agent_Id := Agent.Create_Id_From_Natural (436);
      Added : Boolean := FALSE;
   begin
      Stretch.Set_Id (Stretch_Id);
      District.Add_Stretch (Infrastructure => Stretch_Ref,
                            Added          => Added);
      Stretch.Set_Return_Value_For_Is_Waiting_To_Enter_Stretch (FALSE);

      Ass.Assert (Added,
                  "The stretch is not added to district");

      Ass.Assert (not Stretch_Utils.Is_Waiting_To_Enter_Stretch
                  (Stretch_Id   => Stretch_Id,
                   Traveller_Id => Traveller_Id),
                  "The traveller is waiting to enter stretch");
   end Test_Traveller_Is_Not_Waiting_To_Enter_Stretch;

   procedure Test_Is_Before (T: in out TC.Test_Case'Class)
   is
      Stretch1, Stretch2 : aliased Stretch_Mock.Reference
         := Stretch_Mock.Create;
      Stretch1_Ref : aliased Reactive.Infrastructure.Stretch.Reference
         := Reactive.Infrastructure.Stretch.Reference (Stretch1);
      Stretch2_Ref : aliased Reactive.Infrastructure.Stretch.Reference
         := Reactive.Infrastructure.Stretch.Reference (Stretch2);
      Stretch1_Id : Infra_Id := 345;
      Stretch2_Id : Infra_Id := 323;
      Added : Boolean := FALSE;
   begin
      Stretch1.Set_Id (Stretch1_Id);
      District.Add_Stretch (Infrastructure => Stretch1_Ref,
                            Added          => Added);
      Stretch1.Set_Return_Value_For_Is_Before (TRUE);

      Ass.Assert (Added,
                  "The stretch is not added to district");

      Stretch2.Set_Id (Stretch2_Id);
      District.Add_Stretch (Infrastructure => Stretch2_Ref,
                            Added          => Added);
      Ass.Assert (Added,
                  "The stretch is not added to district");

      Ass.Assert (Stretch_Utils.Is_Before
                  (Left  => Stretch1_Id,
                   Right => Stretch2_Id),
                  "Stretch1 is not before stretch2");
   end Test_Is_Before;

   procedure Test_Is_Not_Before (T: in out TC.Test_Case'Class)
   is
      Stretch1, Stretch2 : aliased Stretch_Mock.Reference
        := Stretch_Mock.Create;
      Stretch1_Ref : aliased Reactive.Infrastructure.Stretch.Reference
         := Reactive.Infrastructure.Stretch.Reference (Stretch1);
      Stretch2_Ref : aliased Reactive.Infrastructure.Stretch.Reference
         := Reactive.Infrastructure.Stretch.Reference (Stretch2);
      Stretch1_Id : Infra_Id := 345;
      Stretch2_Id : Infra_Id := 323;
      Added : Boolean := FALSE;
   begin
      Stretch1.Set_Id (Stretch1_Id);
      District.Add_Stretch (Infrastructure => Stretch1_Ref,
                            Added          => Added);
      Stretch1.Set_Return_Value_For_Is_Before (FALSE);

      Ass.Assert (Added,
                  "The stretch is not added to district");

      Stretch2.Set_Id (Stretch2_Id);
      District.Add_Stretch (Infrastructure => Stretch2_Ref,
                            Added          => Added);
      Ass.Assert (Added,
                  "The stretch is not added to district");

      Ass.Assert (not Stretch_Utils.Is_Before
                  (Left  => Stretch1_Id,
                   Right => Stretch2_Id),
                  "Stretch1 is before stretch2");
   end Test_Is_Not_Before;

   procedure Test_Find_Intersections (T: in out TC.Test_Case'Class)
   is
      Intersections_Expected, Intersections_Found : Infra_Id_Set.Set;
      Stretch : aliased Stretch_Mock.Reference := Stretch_Mock.Create;
      Stretch_Ref : aliased Reactive.Infrastructure.Stretch.Reference
         := Reactive.Infrastructure.Stretch.Reference (Stretch);
      Stretch_Id : Infra_Id := 345;
      Added : Boolean := FALSE;
   begin
      Stretch.Set_Id (Stretch_Id);
      District.Add_Stretch (Infrastructure => Stretch_Ref,
                            Added          => Added);
      Ass.Assert (Added,
                  "The stretch is not added to district");

      Intersections_Expected.Insert (34);
      Intersections_Expected.Insert (73);
      Intersections_Expected.Insert (51);

      Stretch.Set_Return_Value_For_Find_Intersections
        (Intersections_Expected);

      Intersections_Found := Stretch_Utils
        .Find_Intersections (Stretch_Id);

      Ass.Assert (Intersections_Found."=" (Intersections_Expected),
                  "The intersections found are not like the ones expected");
   end Test_Find_Intersections;

   procedure Test_Leave (T: in out TC.Test_Case'Class)
   is
      Stretch : aliased Stretch_Mock.Reference := Stretch_Mock.Create;
      Stretch_Ref : aliased Reactive.Infrastructure.Stretch.Reference
         := Reactive.Infrastructure.Stretch.Reference (Stretch);
      Stretch_Id : Infra_Id := 345;
      Vehicle_Id : Agent.Agent_Id := Agent.Create_Id_From_Natural (437);
      Added, Left : Boolean := FALSE;
   begin
      Stretch.Set_Id (Stretch_Id);
      District.Add_Stretch (Infrastructure => Stretch_Ref,
                            Added          => Added);

      Stretch.Set_Return_Value_For_Leave (TRUE);

      Stretch_Utils.Leave (Stretch_Id   => Stretch_Id,
                           Traveller_Id => Vehicle_Id,
                           Left         => Left);

      Ass.Assert (Left,
                  "The roadway is not leaving by the vehicle");
   end Test_Leave;

   procedure Test_Not_Leave (T: in out TC.Test_Case'Class)
   is
      Stretch : aliased Stretch_Mock.Reference := Stretch_Mock.Create;
      Stretch_Ref : aliased Reactive.Infrastructure.Stretch.Reference
         := Reactive.Infrastructure.Stretch.Reference (Stretch);
      Stretch_Id : Infra_Id := 345;
      Vehicle_Id : Agent.Agent_Id := Agent.Create_Id_From_Natural (438);
      Added, Left : Boolean := FALSE;
   begin
      Stretch.Set_Id (Stretch_Id);
      District.Add_Stretch (Infrastructure => Stretch_Ref,
                            Added          => Added);

      Stretch.Set_Return_Value_For_Leave (FALSe);

      Stretch_Utils.Leave (Stretch_Id   => Stretch_Id,
                           Traveller_Id => Vehicle_Id,
                           Left         => Left);

      Ass.Assert (not Left,
                  "The roadway is leaving by the vehicle");
   end Test_Not_Leave;

   -----------------------------------------------------
   --                  REGISTRATION
   -----------------------------------------------------
   procedure Register_Tests (T: in out Stretch_Utils_Test) is
      use TC.Registration;
   begin
      Register_Routine (Test    => T,
                        Routine => Test_Tread'Access,
                        Name    => "Test tread");

      Register_Routine (Test    => T,
                        Routine => Test_Not_Tread'Access,
                        Name    => "Test not tread");

      Register_Routine (Test    => T,
                        Routine => Test_Id_Getter'Access,
                        Name    => "Test id getter");

      Register_Routine (Test    => T,
                        Routine => Test_Traveller_Is_Waiting_To_Enter_Stretch'Access,
                        Name    => "Test traveller is waiting to enter stretch");

      Register_Routine (Test    => T,
                        Routine => Test_Traveller_Is_Not_Waiting_To_Enter_Stretch'Access,
                        Name    => "Test traveller is not waiting to enter stretch");

      Register_Routine (Test    => T,
                        Routine => Test_Is_Before'Access,
                        Name    => "Test stretch1 is before strech2");

      Register_Routine (Test    => T,
                        Routine => Test_Is_Not_Before'Access,
                        Name    => "Test stretch1 is not before strech2");

      Register_Routine (Test    => T,
                        Routine => Test_Find_Intersections'Access,
                        Name    => "Test intersection finder");

      Register_Routine (Test    => T,
                        Routine => Test_Leave'Access,
                        Name    => "Test leave");

      Register_Routine (Test    => T,
                        Routine => Test_Not_Leave'Access,
                        Name    => "Test not leave");
   end Register_Tests;

   function Name(T: Stretch_Utils_Test) return AU.Message_String is
   begin
      return AU.Format ("Stretch_Utils");
   end Name;
end Reactive.Infrastructure.Stretch.Utils.Tests;
