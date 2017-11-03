with AUnit.Assertions;

with Active.Agent;
with Active.Traveller;
with Active.Traveller.Mock;
with Active.Traveller.Utils.Mock;

with Interface_Layer.Remote.Stub.Mock;
with Interface_Layer.Wrappers.Application.Mock_Factory;

with Reactive.Infrastructure.Mock;
with Reactive.Infrastructure.Intersection.Mock;
with Reactive.Infrastructure.Street.Mock;
with Reactive.Infrastructure.Street_Related_Infrastructure.Mock;
with Reactive.Infrastructure.Way.Mock;
with Reactive.Infrastructure.Way.Roadway.Mock;
with Reactive.Infrastructure.Way.Footway.Mock;
with Reactive.Infrastructure.Way.Bikeway.Mock;
with Reactive.Infrastructure.Lane.Mock;
with Reactive.Infrastructure.Stretch.Mock;
with Reactive.Infrastructure_Registry.Mock;
with Reactive.Traveller_Registry.Mock;
with Reactive.Treadable.Mock;

with Shared.Shared_References_Street;

package body Reactive.District.Tests is
   package Ass
      renames AUnit.Assertions;
   package Agent
      renames Active.Agent;
   package Traveller_Pkg
      renames Active.Traveller;
   package Traveller_Mock
      renames Active.Traveller.Mock;
   package Traveller_Utils_Mock
      renames Active.Traveller.Utils.Mock;
   package Stub_Mock
      renames Interface_Layer.Remote.Stub.Mock;
   package Wrapper_Factory_Mock
      renames Interface_Layer.Wrappers.Application.Mock_Factory;
   package Infrastructure_Mock
      renames Reactive.Infrastructure.Mock;
   package Intersection_Mock
      renames Reactive.Infrastructure.Intersection.Mock;
   package Street_Mock
      renames Reactive.Infrastructure.Street.Mock;
   package Street_Related_Infrastructure_Mock
      renames Reactive.Infrastructure.Street_Related_Infrastructure.Mock;
   package Way_Mock
      renames  Reactive.Infrastructure.Way.Mock;
   package Roadway_Mock
      renames  Reactive.Infrastructure.Way.Roadway.Mock;
   package Footway_Mock
      renames  Reactive.Infrastructure.Way.Footway.Mock;
   package Bikeway_Mock
      renames  Reactive.Infrastructure.Way.Bikeway.Mock;
   package Lane_Mock
      renames  Reactive.Infrastructure.Lane.Mock;
   package Stretch_Mock
      renames  Reactive.Infrastructure.Stretch.Mock;
   package Infrastructure_Registry_Mock
      renames Reactive.Infrastructure_Registry.Mock;
   package Traveller_Registry_Mock
      renames Reactive.Traveller_Registry.Mock;
   package Treadable_Mock
      renames  Reactive.Treadable.Mock;
-- shared
   package SR_Street_Pkg
      renames Shared.Shared_References_Street;

   -----------------------------------------------------
   --                   SCAFFOLDING
   -----------------------------------------------------
   District_Ref            : Reactive.District.Reference;
   Infrastructure_Registry : Infrastructure_Registry_Mock.Reference;
   Traveller_Registry      : Traveller_Registry_Mock.Reference;
   Traveller_Utils         : access Traveller_Utils_Mock.Object;
   Wrapper_Factory         : access Wrapper_Factory_Mock.Object;
   Stub                    : access Stub_Mock.Object;

   procedure Set_Up (T: in out District_Test) is
   begin
      Infrastructure_Registry := Infrastructure_Registry_Mock.Create;
      Traveller_Registry      := Traveller_Registry_Mock.Create;
      Traveller_Utils         := Traveller_Utils_Mock.Create;
      Wrapper_Factory         := Wrapper_Factory_Mock.Create;
      Stub                    := Stub_Mock.Create;

      District_Ref
        := District
          .Get_Instance (Infrastructure_Registry => Infrastructure_Registry,
                         Traveller_Registry      => Traveller_Registry,
                         Traveller_Utils         => Traveller_Utils,
                         App_Wrapper_Factory     => Wrapper_Factory,
                         Stub                    => Stub);
   end Set_Up;

   -----------------------------------------------------
   --                   SCENARIOS
   -----------------------------------------------------

   procedure Test_Infrastructure_Contains (T : in out TC.Test_Case'Class)
   is
      Infrastructure_Id : Infra_Id := 42;
      Added             : Boolean := FALSE;
   begin
      Infrastructure_Registry
         .Set_Return_Value_For_Contains (Infrastructure_Id);

      Ass.Assert (
         District_Ref.Contains_Infrastructure (Infrastructure_Id),
         "The district does not contain the infrastructure id");
   end Test_Infrastructure_Contains;

   procedure Test_Infrastructure_Find (T : in out TC.Test_Case'Class)
   is
      Street_Id : Infra_Id := 63;
      Street : aliased Street_Mock.Reference := Street_Mock.Create;
      Street_Ref : aliased Infrastructure.Reference :=
         Infrastructure.Reference (Street);
      Found_Infrastructure : access Reactive.Infrastructure.Object'Class;
      Added : Boolean := FALSE;
   begin
      Street.Set_Id (Street_Id);

      Infrastructure_Registry
         .Set_Return_Value_For_Find (Street_Id, Street_Ref);

      Found_Infrastructure :=
         District_Ref.Find_Infrastructure_By_Id (Street_Id);

      Ass.Assert (
         Found_Infrastructure = Street_Ref,
         "The district does not contain the infrastructure");
   end Test_Infrastructure_Find;

   procedure Test_Treadable_Find (T : in out TC.Test_Case'Class)
   is
      Treadable_Id  : Infra_Id := 1934;
      Treadable     : aliased Treadable_Mock.Reference :=
         Treadable_Mock.Create;
      Treadable_Ref : aliased Reactive.Treadable.Reference :=
         Reactive.Treadable.Reference (Treadable);
      Found_Treadable : access Reactive.Treadable.Object'Class;
      Added : Boolean := FALSE;
   begin
      Treadable.Set_Return_Value_For_Get_Id (Treadable_Id);

      Infrastructure_Registry
         .Set_Return_Value_For_Find_Treadable (Treadable_Id, Treadable_Ref);

      Found_Treadable := District_Ref
                  .Find_Treadable_By_Id (Treadable_Id);

      Ass.Assert (
         Found_Treadable = Treadable_Ref,
         "The district does not contain the treadable");
   end Test_Treadable_Find;

   procedure Test_Intersection_Find (T : in out TC.Test_Case'Class)
   is
      Intersection_Id    : Infra_Id := 63;
      Intersection       : aliased Intersection_Mock.Reference :=
         Intersection_Mock.Create;
      Intersection_Ref   :
         aliased Reactive.Infrastructure.Intersection.Reference :=
            Reactive.Infrastructure.Intersection.Reference (Intersection);
      Intersection_Infr  : aliased Infrastructure.Reference :=
            Infrastructure.Reference (Intersection);
      Found_Intersection :
         access Reactive.Infrastructure.Intersection.Object'Class;
      Added              : Boolean := FALSE;
   begin
      Intersection.Set_Id (Intersection_Id);

      Infrastructure_Registry
         .Set_Return_Value_For_Find (Intersection_Id, Intersection_Infr);

      Found_Intersection := District_Ref
        .Find_Intersection_By_Id (Intersection_Id);

      Ass.Assert (
         Found_Intersection = Intersection_Ref,
         "The district does not contain the intersection");
   end Test_Intersection_Find;

   procedure Test_Street_Find (T : in out TC.Test_Case'Class)
   is
      Street_Id    : Infra_Id := 1938;
      Street       : aliased Street_Mock.Reference :=
         Street_Mock.Create;
      Street_Ref   :
         aliased Reactive.Infrastructure.Street.Reference :=
            Reactive.Infrastructure.Street.Reference (Street);
      Street_Infr  : aliased Infrastructure.Reference :=
            Infrastructure.Reference (Street);
      Found_Street :
         access Reactive.Infrastructure.Street.Object'Class;
      Added        : Boolean := FALSE;
   begin
      Street.Set_Id (Street_Id);

      Infrastructure_Registry
         .Set_Return_Value_For_Find (Street_Id, Street_Infr);

      Found_Street := District_Ref
        .Find_Street_By_Id (Street_Id);

      Ass.Assert (
         Found_Street = Street_Ref,
         "The district does not contain the street");
   end Test_Street_Find;

   procedure Test_Way_Find (T : in out TC.Test_Case'Class)
   is
      Roadway_Id    : Infra_Id := 1938;
      Roadway       : aliased Roadway_Mock.Reference :=
         Roadway_Mock.Create;
      Roadway_Ref   :
         aliased Reactive.Infrastructure.Way.Roadway.Reference :=
            Reactive.Infrastructure.Way.Roadway.Reference (Roadway);
      Roadway_Infr  : aliased Infrastructure.Reference :=
            Infrastructure.Reference (Roadway);
      Bikeway_Id    : Infra_Id := 1982;
      Bikeway       : aliased Bikeway_Mock.Reference :=
         Bikeway_Mock.Create;
      Bikeway_Ref   :
         aliased Reactive.Infrastructure.Way.Bikeway.Reference :=
            Reactive.Infrastructure.Way.Bikeway.Reference (Bikeway);
      Bikeway_Infr  : aliased Infrastructure.Reference :=
            Infrastructure.Reference (Bikeway);
      Footway_Id    : Infra_Id := 2006;
      Footway       : aliased Footway_Mock.Reference :=
         Footway_Mock.Create;
      Footway_Ref   :
         aliased Reactive.Infrastructure.Way.Footway.Reference :=
            Reactive.Infrastructure.Way.Footway.Reference (Footway);
      Footway_Infr  : aliased Infrastructure.Reference :=
            Infrastructure.Reference (Footway);
      Found_Way     : access Reactive.Infrastructure.Way.Object'Class;
      Added         : Boolean := FALSE;
   begin
      Roadway.Set_Id (Roadway_Id);
      Bikeway.Set_Id (Bikeway_Id);
      Footway.Set_Id (Footway_Id);

      Infrastructure_Registry
         .Set_Return_Value_For_Find (Roadway_Id, Roadway_Infr);

      Infrastructure_Registry
         .Set_Return_Value_For_Find (Bikeway_Id, Bikeway_Infr);

      Infrastructure_Registry
         .Set_Return_Value_For_Find (Footway_Id, Footway_Infr);

      Found_Way := District_Ref
        .Find_Way_By_Id (Roadway_Id);

      Ass.Assert (Found_Way = Roadway_Ref,
                  "The infrastructure registry does not "
                  & "contains the roadway");

      Found_Way := District_Ref
        .Find_Way_By_Id (Bikeway_Id);

      Ass.Assert (Found_Way = Bikeway_Ref,
                  "The infrastructure registry does not "
                  & "contains the bikeway");

      Found_Way := District_Ref
        .Find_Way_By_Id (Footway_Id);

      Ass.Assert (Found_Way = Footway_Ref,
                  "The infrastructure registry does not "
                  & "contains the footway");
   end Test_Way_Find;

   procedure Test_Roadway_Find (T : in out TC.Test_Case'Class)
   is
      Roadway_Id    : Infra_Id := 45;
      Roadway       : aliased Roadway_Mock.Reference :=
         Roadway_Mock.Create;
      Roadway_Ref   :
         aliased Reactive.Infrastructure.Way.Roadway.Reference :=
            Reactive.Infrastructure.Way.Roadway.Reference (Roadway);
      Roadway_Infr  : aliased Infrastructure.Reference :=
            Infrastructure.Reference (Roadway);
      Found_Roadway : access Reactive.Infrastructure.Way.Roadway.Object'Class;
      Added         : Boolean := FALSE;
   begin
      Roadway.Set_Id (Roadway_Id);

      Infrastructure_Registry
         .Set_Return_Value_For_Find (Roadway_Id, Roadway_Infr);

      Found_Roadway := District_Ref
        .Find_Roadway_By_Id (Roadway_Id);

      Ass.Assert (Found_Roadway = Roadway_Ref,
                  "The infrastructure registry does not "
                  & "contains the road way");
   end Test_Roadway_Find;

   procedure Test_Footway_Find (T : in out TC.Test_Case'Class)
   is
      Footway_Id    : Infra_Id := 45;
      Footway       : aliased Footway_Mock.Reference :=
         Footway_Mock.Create;
      Footway_Ref   :
         aliased Reactive.Infrastructure.Way.Footway.Reference :=
            Reactive.Infrastructure.Way.Footway.Reference (Footway);
      Footway_Infr  : aliased Infrastructure.Reference :=
            Infrastructure.Reference (Footway);
      Found_Footway : access Reactive.Infrastructure.Way.Footway.Object'Class;
      Added         : Boolean := FALSE;
   begin
      Footway.Set_Id (Footway_Id);

      Infrastructure_Registry
         .Set_Return_Value_For_Find (Footway_Id, Footway_Infr);

      Found_Footway := District_Ref
         .Find_Footway_By_Id (Footway_Id);

      Ass.Assert (Found_Footway = Footway_Ref,
                  "The infrastructure registry does not "
                  & "contains the footway");
   end Test_Footway_Find;

   procedure Test_Bikeway_Find (T : in out TC.Test_Case'Class)
   is
      Bikeway_Id    : Infra_Id := 45;
      Bikeway       : aliased Bikeway_Mock.Reference :=
         Bikeway_Mock.Create;
      Bikeway_Ref   :
         aliased Reactive.Infrastructure.Way.Bikeway.Reference :=
            Reactive.Infrastructure.Way.Bikeway.Reference (Bikeway);
      Bikeway_Infr  : aliased Infrastructure.Reference :=
            Infrastructure.Reference (Bikeway);
      Found_Bikeway : access Reactive.Infrastructure.Way.Bikeway.Object'Class;
      Added         : Boolean := FALSE;
   begin
      Bikeway.Set_Id (Bikeway_Id);

      Infrastructure_Registry
         .Set_Return_Value_For_Find (Bikeway_Id, Bikeway_Infr);

      Found_Bikeway := District_Ref
        .Find_Bikeway_By_Id (Bikeway_Id);

      Ass.Assert (Found_Bikeway = Bikeway_Ref,
                  "The infrastructure registry does not "
                  & "contains the bikeway");
   end Test_Bikeway_Find;

   procedure Test_Lane_Find (T : in out TC.Test_Case'Class)
   is
      Lane_Id    : Infra_Id := 45;
      Lane       : aliased Lane_Mock.Reference :=
         Lane_Mock.Create;
      Lane_Ref   : aliased Reactive.Infrastructure.Lane.Reference :=
         Reactive.Infrastructure.Lane.Reference (Lane);
      Lane_Infr  : aliased Infrastructure.Reference :=
         Infrastructure.Reference (Lane);
      Found_Lane : access Reactive.Infrastructure.Lane.Object'Class;
      Added      : Boolean := FALSE;
   begin
      Lane.Set_Id (Lane_Id);

      Infrastructure_Registry.Set_Return_Value_For_Find (Lane_Id, Lane_Infr);

      Found_Lane := District_Ref.Find_Lane_By_Id (Lane_Id);

      Ass.Assert (Found_Lane = Lane_Ref,
                  "The infrastructure registry does not "
                  & "contains the lane");
   end Test_Lane_Find;

   procedure Test_Stretch_Find (T : in out TC.Test_Case'Class)
   is
      Stretch_Id    : Infra_Id := 45;
      Stretch       : aliased Stretch_Mock.Reference :=
         Stretch_Mock.Create;
      Stretch_Ref   : aliased Reactive.Infrastructure.Stretch.Reference :=
         Reactive.Infrastructure.Stretch.Reference (Stretch);
      Stretch_Infr  : aliased Infrastructure.Reference :=
         Infrastructure.Reference (Stretch);
      Found_Stretch : access Reactive.Infrastructure.Stretch.Object'Class;
      Added : Boolean := FALSE;
   begin
      Stretch.Set_Id (Stretch_Id);

      Infrastructure_Registry
         .Set_Return_Value_For_Find (Stretch_Id, Stretch_Infr);

      Found_Stretch := District_Ref
        .Find_Stretch_By_Id (Stretch_Id);

      Ass.Assert (Found_Stretch = Stretch_Ref,
                  "The infrastructure registry does not "
                  & "contains the stretch");
   end Test_Stretch_Find;

   procedure Test_Traveller_Find (T : in out TC.Test_Case'Class)
   is
      Traveller_Id  : Agent.Agent_Id := Agent.Create_Id_From_Natural (63);
      Traveller_Obj : aliased Traveller_Mock.Object'Class
        := Traveller_Mock.Create.all;
      Traveller_Ref : Traveller_Pkg.Reference :=
         Traveller_Obj'Unchecked_Access;
      Found_Traveller : access Active.Traveller.Object'Class;
      Added : Boolean := FALSE;
   begin
      Traveller_Obj.Set_Id (Traveller_Id);

      Ass.Assert (not Traveller_Registry.Contains_Traveller (Traveller_Id),
                  "The traveller registry already contains the traveller");

      Traveller_Registry.Add_Traveller (
         Traveller => Traveller_Ref,
         Added     => Added);

      Found_Traveller := District_Ref.Find_Traveller_By_Id (Traveller_Id);

      Ass.Assert (
         Found_Traveller = Traveller_Obj'Access,
         "The traveller registry does not contains the traveller");
   end Test_Traveller_Find;

   procedure Test_Traveller_Add (T : in out TC.Test_Case'Class)
   is
      Traveller_Id : Agent.Agent_Id := Agent.Create_Id_From_Natural (64);
      Traveller_Obj : aliased Traveller_Mock.Object'Class
        := Traveller_Mock.Create.all;
      Traveller_Ref : Traveller_Pkg.Reference :=
         Traveller_Obj'Unchecked_Access;
      Found_Traveller : access Active.Traveller.Object'Class;
      Added : Boolean := FALSE;
   begin
      Traveller_Obj.Set_Id (Traveller_Id);

      Ass.Assert (not Traveller_Registry.Contains_Traveller (Traveller_Id),
                  "The traveller registry already contains the traveller");

      District_Ref
        .Add_Traveller (Traveller => Traveller_Ref,
                        Added     => Added);

      Found_Traveller :=
         Traveller_Registry.Find_Traveller_By_Id (Traveller_Id);

      Ass.Assert (
         Found_Traveller = Traveller_Obj'Access,
         "The traveller registry does not contains the traveller");
   end Test_Traveller_Add;

   procedure Test_Traveller_Remove (T : in out TC.Test_Case'Class)
   is
      Traveller_Id : Agent.Agent_Id := Agent.Create_Id_From_Natural (65);
      Traveller_Obj : aliased Traveller_Mock.Object'Class
        := Traveller_Mock.Create.all;
      Traveller_Ref : Traveller_Pkg.Reference :=
         Traveller_Obj'Unchecked_Access;
      Added, Removed : Boolean := FALSE;
   begin
      Traveller_Obj.Set_Id (Traveller_Id);

      Ass.Assert (not Traveller_Registry.Contains_Traveller (Traveller_Id),
                  "The traveller registry already contains the traveller");

      Traveller_Registry.Add_Traveller (
         Traveller => Traveller_Ref,
         Added     => Added);

      Ass.Assert (Added,
                  "The traveller is not added into the registry");

      Ass.Assert (Traveller_Registry.Contains_Traveller (Traveller_Id),
                  "The traveller registry does not contains the traveller id");

      District_Ref.Remove_Traveller (Traveller_Id => Traveller_Id,
                                     Removed      => Removed);

      Ass.Assert (
         not Traveller_Registry.Contains_Traveller (Traveller_Id),
         "The traveller registry does no more contains the traveller id");
   end Test_Traveller_Remove;

   procedure Test_Intersection_Add (T : in out TC.Test_Case'Class)
   is
      Intersection_Id : Infra_Id := 4567;
      Intersection : aliased Intersection_Mock.Object'Class
        := Intersection_Mock.Create.all;
      Added : Boolean := FALSE;
   begin
      Intersection.Set_Id (Intersection_Id);

      District_Ref
        .Add_Intersection (Infrastructure => Intersection,
                           Added          => Added);

      Ass.Assert (Added, "'Added' flag was not set to true");

      Ass.Assert (
         Infrastructure_Registry.Contains_Infrastructure (Intersection_Id),
         "The infrastructure registry does not contain the intersection");
   end Test_Intersection_Add;

   procedure Test_Street_Add (T : in out TC.Test_Case'Class)
   is
      Street_Id  : Infra_Id := 4567;
      SR_Street  : SR_Street_Pkg.Shared_Reference;
      Street     : aliased Street_Mock.Reference := Street_Mock.Create;
      Street_Ref : aliased Reactive.Infrastructure.Street.Reference
         := Reactive.Infrastructure.Street.Reference (Street);
      Added      : Boolean := FALSE;
   begin
      SR_Street.Init (SR_Street_Pkg.T_Reference (Street_Ref));
      Street.Set_Id (Street_Id);

      District_Ref
        .Add_Street (SR_Street => SR_Street,
                     Added     => Added);

      Ass.Assert (Added, "'Added' flag was not set to true");

      Ass.Assert (
         Infrastructure_Registry.Contains_Infrastructure (Street_Id),
         "The infrastructure registry does not contain the street");
   end Test_Street_Add;

   procedure Test_Roadway_Add (T : in out TC.Test_Case'Class)
   is
      Roadway_Id : Infra_Id := 4567;
      Roadway : aliased Roadway_Mock.Reference
        := Roadway_Mock.Create;
      Roadway_Ref : aliased Reactive.Infrastructure.Way.Roadway.Reference
         := Reactive.Infrastructure.Way.Roadway.Reference (Roadway);
      Added : Boolean := FALSE;
   begin
      Roadway.Set_Id (Roadway_Id);

      District_Ref
        .Add_Roadway (Infrastructure => Roadway_Ref,
                      Added          => Added);

      Ass.Assert (Added, "'Added' flag was not set to true");

      Ass.Assert (
         Infrastructure_Registry.Contains_Infrastructure (Roadway_Id),
         "The infrastructure registry does not contain the roadway");
   end Test_Roadway_Add;

   procedure Test_Footway_Add (T : in out TC.Test_Case'Class)
   is
      Footway_Id : Infra_Id := 4567;
      Footway : aliased Footway_Mock.Reference
        := Footway_Mock.Create;
      Footway_Ref : aliased Reactive.Infrastructure.Way.Footway.Reference
         := Reactive.Infrastructure.Way.Footway.Reference (Footway);
      Added : Boolean := FALSE;
   begin
      Footway.Set_Id (Footway_Id);

      District_Ref
        .Add_Footway (Infrastructure => Footway_Ref,
                      Added          => Added);

      Ass.Assert (Added, "'Added' flag was not set to true");

      Ass.Assert (
         Infrastructure_Registry.Contains_Infrastructure (Footway_Id),
         "The infrastructure registry does not contain the footway");
   end Test_Footway_Add;

   procedure Test_Bikeway_Add (T : in out TC.Test_Case'Class)
   is
      Bikeway_Id : Infra_Id := 4567;
      Bikeway : aliased Bikeway_Mock.Reference
        := Bikeway_Mock.Create;
      Bikeway_Ref : aliased Reactive.Infrastructure.Way.Bikeway.Reference
         := Reactive.Infrastructure.Way.Bikeway.Reference (Bikeway);
      Added : Boolean := FALSE;
   begin
      Bikeway.Set_Id (Bikeway_Id);

      District_Ref
        .Add_Bikeway (Infrastructure => Bikeway_Ref,
                      Added          => Added);

      Ass.Assert (Added, "'Added' flag was not set to true");

      Ass.Assert (
         Infrastructure_Registry.Contains_Infrastructure (Bikeway_Id),
         "The infrastructure registry does not contain the Bikeway");
   end Test_Bikeway_Add;

   procedure Test_Lane_Add (T : in out TC.Test_Case'Class)
   is
      Lane_Id : Infra_Id := 4567;
      Lane : aliased Lane_Mock.Reference
        := Lane_Mock.Create;
      Lane_Ref : aliased Reactive.Infrastructure.Lane.Reference
         := Reactive.Infrastructure.Lane.Reference (Lane);
      Added : Boolean := FALSE;
   begin
      Lane.Set_Id (Lane_Id);

      District_Ref
        .Add_Lane (Infrastructure => Lane_Ref,
                   Added          => Added);

      Ass.Assert (Added, "'Added' flag was not set to true");

      Ass.Assert (
         Infrastructure_Registry.Contains_Infrastructure (Lane_Id),
         "The infrastructure registry does not contain the Lane");
   end Test_Lane_Add;

   procedure Test_Stretch_Add (T : in out TC.Test_Case'Class)
   is
      Stretch_Id : Infra_Id := 4567;
      Stretch : aliased Stretch_Mock.Reference
        := Stretch_Mock.Create;
      Stretch_Ref : aliased Reactive.Infrastructure.Stretch.Reference
         := Reactive.Infrastructure.Stretch.Reference (Stretch);
      Added : Boolean := FALSE;
   begin
      Stretch.Set_Id (Stretch_Id);

      District_Ref
        .Add_Stretch (Infrastructure => Stretch_Ref,
                      Added          => Added);

      Ass.Assert (Added, "'Added' flag was not set to true");

      Ass.Assert (
         Infrastructure_Registry.Contains_Infrastructure (Stretch_Id),
         "The infrastructure registry does not contain the Stretch");
   end Test_Stretch_Add;

   procedure Test_Try_To_Tread_Loc (T : in out TC.Test_Case'Class)
   is
      Treadable_Id    : Infra_Id := 2019;
      Treadable       : aliased Treadable_Mock.Reference :=
         Treadable_Mock.Create;
      Treadable_Ref   : aliased Reactive.Treadable.Reference :=
         Reactive.Treadable.Reference (Treadable);
      Traveller_Id    : Agent.Agent_Id := Agent.Create_Id_From_Natural (2020);
      Traveller_Obj : aliased Traveller_Mock.Object'Class
        := Traveller_Mock.Create.all;
      Traveller_Ref : Traveller_Pkg.Reference :=
         Traveller_Obj'Unchecked_Access;
      Added        : Boolean := FALSE;
      Advanced        : Boolean := FALSE;
   begin
      Treadable.Set_Return_Value_For_Get_Id (Treadable_Id);
      Traveller_Obj.Set_Id (Traveller_Id);

      Treadable.Set_Return_Value_For_Tread (True);

      Infrastructure_Registry
         .Set_Return_Value_For_Contains (Treadable_Id);
      Infrastructure_Registry
         .Set_Return_Value_For_Find_Treadable (Treadable_Id, Treadable_Ref);

      Traveller_Registry.Add_Traveller (Traveller => Traveller_Ref,
                                        Added     => Added);

      District_Ref.Try_To_Tread_Infrastructure (
         Traveller_Id, Treadable_Id, Advanced);

      Ass.Assert (
         Advanced,
         "The traveller did not tread the local infrastructure");

      Ass.Assert (
         Traveller_Utils.Get_Consume_Step_Called,
         "The traveller did not consume a step of its travel");
   end Test_Try_To_Tread_Loc;

   procedure Test_Try_To_Tread_Rem (T : in out TC.Test_Case'Class)
   is
      Treadable_Id    : Infra_Id := 2017;
      Traveller_Id    : Agent.Agent_Id := Agent.Create_Id_From_Natural (2018);
      Traveller_Obj   : aliased Traveller_Mock.Object'Class
        := Traveller_Mock.Create.all;
      Traveller_Ref   : Traveller_Pkg.Reference :=
         Traveller_Obj'Unchecked_Access;
      Added           : Boolean := FALSE;
      Advanced        : Boolean := FALSE;
   begin
      Traveller_Obj.Set_Id (Traveller_Id);

      Traveller_Registry.Add_Traveller (Traveller => Traveller_Ref,
                                        Added     => Added);

      Stub.Set_Return_Value_For_Enter (True);

      District_Ref.Try_To_Tread_Infrastructure (
         Traveller_Id, Treadable_Id, Advanced);

      Ass.Assert (
         not Advanced,
         "The Tread operation was not delegated to the callback pair");
   end Test_Try_To_Tread_Rem;

   -----------------------------------------------------
   --                  REGISTRATION
   -----------------------------------------------------
   procedure Register_Tests (T: in out District_Test) is
      use TC.Registration;
   begin
      Register_Routine (Test    => T,
                        Routine => Test_Infrastructure_Contains'Access,
                        Name    => "Test infrastructure contains");

      Register_Routine (Test    => T,
                        Routine => Test_Infrastructure_Find'Access,
                        Name    => "Test infrastructure find");

      Register_Routine (Test    => T,
                        Routine => Test_Intersection_Find'Access,
                        Name    => "Test intersection find");

      Register_Routine (Test    => T,
                        Routine => Test_Street_Find'Access,
                        Name    => "Test street find");

      Register_Routine (Test    => T,
                        Routine => Test_Way_Find'Access,
                        Name    => "Test way find");

      Register_Routine (Test    => T,
                        Routine => Test_Roadway_Find'Access,
                        Name    => "Test roadway find");

      Register_Routine (Test    => T,
                        Routine => Test_Footway_Find'Access,
                        Name    => "Test footway find");

      Register_Routine (Test    => T,
                        Routine => Test_Bikeway_Find'Access,
                        Name    => "Test bikeway find");

      Register_Routine (Test    => T,
                        Routine => Test_Lane_Find'Access,
                        Name    => "Test lane find");

      Register_Routine (Test    => T,
                        Routine => Test_Stretch_Find'Access,
                        Name    => "Test stretch find");

      Register_Routine (Test    => T,
                        Routine => Test_Traveller_Find'Access,
                        Name    => "Test traveller find");

      Register_Routine (Test    => T,
                        Routine => Test_Traveller_Add'Access,
                        Name    => "Test traveller add");

      Register_Routine (Test    => T,
                        Routine => Test_Traveller_Remove'Access,
                        Name    => "Test traveller remove");

      Register_Routine (Test    => T,
                        Routine => Test_Intersection_Add'Access,
                        Name    => "Test intersection add");

      Register_Routine (Test    => T,
                        Routine => Test_Street_Add'Access,
                        Name    => "Test street add");

      Register_Routine (Test    => T,
                        Routine => Test_Roadway_Add'Access,
                        Name    => "Test roadway add");

      Register_Routine (Test    => T,
                        Routine => Test_Footway_Add'Access,
                        Name    => "Test footway add");

      Register_Routine (Test    => T,
                        Routine => Test_Bikeway_Add'Access,
                        Name    => "Test bikeway add");

      Register_Routine (Test    => T,
                        Routine => Test_Lane_Add'Access,
                        Name    => "Test lane add");

      Register_Routine (Test    => T,
                        Routine => Test_Stretch_Add'Access,
                        Name    => "Test stretch add");

      Register_Routine (Test    => T,
                        Routine => Test_Try_To_Tread_Loc'Access,
                        Name    => "Test local try to enter infrastructure");
   end Register_Tests;

   function Name(T: District_Test) return AU.Message_String is
   begin
      return AU.Format ("Reactive.District");
   end Name;
end Reactive.District.Tests;
