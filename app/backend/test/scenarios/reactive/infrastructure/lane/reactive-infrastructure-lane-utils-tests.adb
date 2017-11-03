with AUnit.Assertions;
with Ada.Text_IO;

with Active.Agent;

with Reactive.District.Mock;
with Reactive.Infrastructure.Lane.Mock;

package body Reactive.Infrastructure.Lane.Utils.Tests is
   package Ass renames AUnit.Assertions;
   package Agent renames Active.Agent;
   package District_Mock renames Reactive.District.Mock;
   package Lane_Mock renames Reactive.Infrastructure.Lane.Mock;

   -----------------------------------------------------
   --                   SCAFFOLDING
   -----------------------------------------------------
   Lane_Utils : Reactive.Infrastructure.Lane.Utils.Reference;
   District : access District_Mock.Object;

   procedure Set_Up (T: in out Lane_Utils_Test) is
   begin
      District := District_Mock.Create;

      Lane_Utils
        := Reactive.Infrastructure.Lane.Utils.Get_Instance (District => District);
   end Set_Up;

   procedure Test_Intersections_Finder (T: in out TC.Test_Case'Class)
   is
      Intersections_Expected, Intersections_Found : Infra_Id_Set.Set;
      Lane1 : access Lane_Mock.Object'Class
        := Lane_Mock.Create;
      Lane2 : access Lane_Mock.Object'Class
        := Lane_Mock.Create;
      Lane1_Ref : aliased Reactive.Infrastructure.Lane.Reference
         := Reactive.Infrastructure.Lane.Reference (Lane1);
      Lane2_Ref : aliased Reactive.Infrastructure.Lane.Reference
         := Reactive.Infrastructure.Lane.Reference (Lane2);
      Lane1_Id : Infra_Id := 345;
      Added : Boolean := FALSE;
   begin
      Lane1.Set_Id (Lane1_Id);
      District.Add_Lane (Infrastructure => Lane1_Ref,
                         Added          => Added);
      Ass.Assert (Added, "The Lane is not added to district");

      Intersections_Expected.Insert (34);
      Intersections_Expected.Insert (73);
      Intersections_Expected.Insert (51);

      Lane1.Set_Return_Value_For_Find_Intersections
        (Intersections_Expected);

      Intersections_Found := Lane_Utils
        .Find_Intersections (Lane1_Id);

      Ass.Assert (Intersections_Found."=" (Intersections_Expected),
                  "The intersections found are not like the ones expected");
   end Test_Intersections_Finder;

   procedure Test_Is_Contained_By (T: in out TC.Test_Case'Class)
   is
      Lane : access Lane_Mock.Object'Class
        := Lane_Mock.Create;
      Lane_Ref : aliased Reactive.Infrastructure.Lane.Reference
         := Reactive.Infrastructure.Lane.Reference (Lane);
      Lane_Id : Infra_Id := 345;
      Container_Id : Infra_Id := 1345;
      Added : Boolean := FALSE;
   begin
      Lane.Set_Id (Lane_Id);
      District.Add_Lane (Infrastructure => Lane_Ref,
                         Added          => Added);

      Ass.Assert (Added,
                  "The lane is not added to district");

      Lane.Set_Return_Value_For_Is_Contained_By (TRUE);

      Ass.Assert (Lane_Utils
                  .Is_Contained_By (Lane_Id      => Lane_Id,
                                    Container_Id => Container_Id),
                  "The way is not contained by the containter");
   end Test_Is_Contained_By;

   procedure Test_Is_Not_Contained_By (T: in out TC.Test_Case'Class)
   is
      Lane : access Lane_Mock.Object'Class
        := Lane_Mock.Create;
      Lane_Ref : aliased Reactive.Infrastructure.Lane.Reference
         := Reactive.Infrastructure.Lane.Reference (Lane);
      Lane_Id : Infra_Id := 345;
      Container_Id : Infra_Id := 1345;
      Added : Boolean := FALSE;
   begin
      Lane.Set_Id (Lane_Id);
      District.Add_Lane (Infrastructure => Lane_Ref,
                         Added          => Added);

      Ass.Assert (Added,
                  "The lane is not added to district");

      Lane.Set_Return_Value_For_Is_Contained_By (FALSE);

      Ass.Assert (not Lane_Utils
                  .Is_Contained_By (Lane_Id      => Lane_Id,
                                    Container_Id => Container_Id),
                  "The way is contained by the containter");
   end Test_Is_Not_Contained_By;

   procedure Test_Street_Finder (T: in out TC.Test_Case'Class)
   is
      Lane : access Lane_Mock.Object'Class
        := Lane_Mock.Create;
      Lane_Ref : aliased Reactive.Infrastructure.Lane.Reference
         := Reactive.Infrastructure.Lane.Reference (Lane);
      Lane_Id : Infra_Id := 345;
      Street_Id : Infra_Id := 34;
      Added : Boolean := FALSE;
   begin
      Lane.Set_Id (Lane_Id);
      District.Add_Lane (Infrastructure => Lane_Ref,
                         Added          => Added);

      Ass.Assert (Added,
                  "The lane is not added to district");

      Lane.Set_Return_Value_For_Find_Street (Street_Id);

      Ass.Assert (Lane_Utils.Find_Street (Lane_Id => Lane_Id)
                  = Street_Id,
                  "The value returned by Stretch id getter is wrong");
   end Test_Street_Finder;

   procedure Test_Stretches_Counter (T: in out TC.Test_Case'Class)
   is
      Lane : access Lane_Mock.Object'Class
        := Lane_Mock.Create;
      Lane_Ref : aliased Reactive.Infrastructure.Lane.Reference
         := Reactive.Infrastructure.Lane.Reference (Lane);
      Lane_Id : Infra_Id := 345;
      Stretches_Count : Natural := 34;
      Added : Boolean := FALSE;
   begin
      Lane.Set_Id (Lane_Id);
      District.Add_Lane (Infrastructure => Lane_Ref,
                         Added          => Added);

      Ass.Assert (Added,
                  "The lane is not added to district");

      Lane.Set_Return_Value_For_Count_Stretches (Stretches_Count);

      Ass.Assert (Lane_Utils.Count_Stretches (Lane_Id => Lane_Id)
                  = Stretches_Count,
                  "The value returned by Stretches counter is wrong");
   end Test_Stretches_Counter;

   procedure Test_Direction_Getter (T: in out TC.Test_Case'Class)
   is
      Lane : access Lane_Mock.Object'Class
        := Lane_Mock.Create;
      Lane_Ref : aliased Reactive.Infrastructure.Lane.Reference
         := Reactive.Infrastructure.Lane.Reference (Lane);
      Lane_Id : Infra_Id := 345;
      Direction : Shared.Direction.Straight := Shared.Direction.NORTH_SOUTH;
      Added : Boolean := FALSE;
   begin
      Lane.Set_Id (Lane_Id);
      District.Add_Lane (Infrastructure => Lane_Ref,
                         Added          => Added);

      Ass.Assert (Added,
                  "The lane is not added to district");

      Lane.Set_Direction (Direction);

      Ass.Assert (Shared.Direction
                  ."=" (Lane_Utils.Get_Direction (Lane_Id => Lane_Id),
                    Direction),
                  "The value returned by direction getter is wrong");
   end Test_Direction_Getter;

   procedure Test_Enter (T: in out TC.Test_Case'Class)
   is
      Lane : access Lane_Mock.Object'Class
        := Lane_Mock.Create;
      Lane_Ref : aliased Reactive.Infrastructure.Lane.Reference
         := Reactive.Infrastructure.Lane.Reference (Lane);
      Lane_Id : Infra_Id := 345;
      Stretch_Id : Infra_Id := 23;
      Traveller_Id : Agent.Agent_Id := Agent.Create_Id_From_Natural (539);
      Added, Entered : Boolean := FALSE;
   begin
      Lane.Set_Id (Lane_Id);
      District.Add_Lane (Infrastructure => Lane_Ref,
                         Added          => Added);

      Ass.Assert (Added,
                  "The lane is not added to district");

   -- TODO: Does this test make sense anymore?
   end Test_Enter;

   procedure Test_Not_Enter (T: in out TC.Test_Case'Class)
   is
      Lane : access Lane_Mock.Object'Class
        := Lane_Mock.Create;
      Lane_Ref : aliased Reactive.Infrastructure.Lane.Reference
         := Reactive.Infrastructure.Lane.Reference (Lane);
      Lane_Id : Infra_Id := 345;
      Stretch_Id : Infra_Id := 23;
      Traveller_Id : Agent.Agent_Id := Agent.Create_Id_From_Natural (538);
      Added, Entered : Boolean := FALSE;
   begin
      Lane.Set_Id (Lane_Id);
      District.Add_Lane (Infrastructure => Lane_Ref,
                         Added          => Added);

      Ass.Assert (Added,
                  "The lane is not added to district");

   -- TODO: Does this test make sense anymore?
   end Test_Not_Enter;

   procedure Test_Find_Stretch_Position (T: in out TC.Test_Case'Class)
   is
      Lane : access Lane_Mock.Object'Class
        := Lane_Mock.Create;
      Lane_Ref : aliased Reactive.Infrastructure.Lane.Reference
         := Reactive.Infrastructure.Lane.Reference (Lane);
      Lane_Id : Infra_Id := 345;
      Stretch_Id : Infra_Id := 23;
      Added, Found : Boolean := FALSE;
      Found_Stretch_Position : Natural;
      Expected_Stretch_Position : Natural := 473;
   begin
      Lane.Set_Id (Lane_Id);
      District.Add_Lane (Infrastructure => Lane_Ref,
                         Added          => Added);

      Ass.Assert (Added,
                  "The lane is not added to district");

      Lane.Set_Return_Value_For_Find_Stretch_Position
        (Position => Expected_Stretch_Position,
         Found => TRUE);

      Lane_Utils.Find_Stretch_Position
        (Lane_Id => Lane_Id,
         Stretch_Id => Stretch_Id,
         Stretch_Position => Found_Stretch_Position,
         Found   => Found);

      Ass.Assert (Found,
                  "The stretch position is not found");

      Ass.Assert (Found_Stretch_Position = Expected_Stretch_Position,
                  "The stretch position is like expected");
   end Test_Find_Stretch_Position;

   procedure Test_Intersection_Adder (T: in out TC.Test_Case'Class)
   is
      Lane : access Lane_Mock.Object'Class
        := Lane_Mock.Create;
      Lane_Ref : aliased Reactive.Infrastructure.Lane.Reference
         := Reactive.Infrastructure.Lane.Reference (Lane);
      Lane_Id : Infra_Id := 345;
      Intersection_Id : Infra_Id := 23;
      Added : Boolean := FALSE;
   begin
      Lane.Set_Id (Lane_Id);
      District.Add_Lane (Infrastructure => Lane_Ref,
                         Added          => Added);

      Ass.Assert (Added,
                  "The lane is not added to district");

      Lane.Set_Return_Value_For_Add_Intersection (TRUE);

      Lane_Utils.Add_Intersection (Lane_Id         => Lane_Id,
                                   Intersection_Id => Intersection_Id,
                                   Added           => Added);

      Ass.Assert (Added,
                  "The intersection is not added");
   end Test_Intersection_Adder;

   -----------------------------------------------------
   --                  REGISTRATION
   -----------------------------------------------------
   procedure Register_Tests (T: in out Lane_Utils_Test) is
      use TC.Registration;
   begin

      Register_Routine (Test    => T,
                        Routine => Test_Intersections_Finder'Access,
                        Name    => "Test intersection finder");

      Register_Routine (Test => T,
                        Routine => Test_Is_Contained_By'Access,
                        Name => "Test way is contained by a container");

      Register_Routine (Test => T,
                        Routine => Test_Is_Not_Contained_By'Access,
                        Name => "Test way is not contained by a container");

      Register_Routine (Test => T,
                        Routine => Test_Street_Finder'Access,
                        Name => "Test street finder");

      Register_Routine (Test => T,
                        Routine => Test_Stretches_Counter'Access,
                        Name => "Test stretches counter");

      Register_Routine (Test => T,
                        Routine => Test_Direction_Getter'Access,
                        Name => "Test direction getter");

      Register_Routine (Test    => T,
                        Routine => Test_Enter'Access,
                        Name    => "Test enter");

      Register_Routine (Test    => T,
                        Routine => Test_Not_Enter'Access,
                        Name    => "Test not enter");

      Register_Routine (Test    => T,
                        Routine => Test_Find_Stretch_Position'Access,
                        Name    => "Test stretch position finder");

      Register_Routine (Test    => T,
                        Routine => Test_Intersection_Adder'Access,
                        Name    => "Test intersection adder");
   end Register_Tests;

   function Name(T: Lane_Utils_Test) return AU.Message_String is
   begin
      return AU.Format ("Lane_Utils");
   end Name;
end Reactive.Infrastructure.Lane.Utils.Tests;
