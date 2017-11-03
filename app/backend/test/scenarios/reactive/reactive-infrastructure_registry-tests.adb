with AUnit.Assertions;
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

with Shared.Shared_References_Street;

package body Reactive.Infrastructure_Registry.Tests is
   package Ass renames AUnit.Assertions;
   package Infrastructure_Mock renames Reactive.Infrastructure.Mock;
   package Intersection_Mock renames Reactive.Infrastructure.Intersection.Mock;
   package Street_Mock renames Reactive.Infrastructure.Street.Mock;
   package Street_Related_Infrastructure_Mock
      renames Reactive.Infrastructure.Street_Related_Infrastructure.Mock;
   package Way_Mock renames  Reactive.Infrastructure.Way.Mock;
   package Roadway_Mock renames  Reactive.Infrastructure.Way.Roadway.Mock;
   package Footway_Mock renames  Reactive.Infrastructure.Way.Footway.Mock;
   package Bikeway_Mock renames  Reactive.Infrastructure.Way.Bikeway.Mock;
   package Lane_Mock renames  Reactive.Infrastructure.Lane.Mock;
   package Stretch_Mock renames  Reactive.Infrastructure.Stretch.Mock;
-- shared
   package SR_Street_Pkg renames Shared.Shared_References_Street;

   use Reactive.Infrastructure;

   -----------------------------------------------------
   --                   SCAFFOLDING
   -----------------------------------------------------
   Infrastructure_Registry_Ref : Reactive.Infrastructure_Registry.Reference;

   procedure Set_Up_Case (T: in out Infrastructure_Registry_Test) is
   begin
      Infrastructure_Registry_Ref
        := Infrastructure_Registry.Get_Instance;
   end Set_Up_Case;

   procedure Tear_Down (T: in out Infrastructure_Registry_Test) is
   begin
      -- Clear registry
      Infrastructure_Registry_Ref.Clear;
   end Tear_Down;

   -----------------------------------------------------
   --                   SCENARIOS
   -----------------------------------------------------

   procedure Test_Infrastructure_Contains (T : in out TC.Test_Case'Class)
   is
      Street_Id       : Infra_Id := 42;
      Street_Mock_Ref : Street_Mock.Reference := Street_Mock.Create;
      Street          : aliased Reactive.Infrastructure.Street.Reference
        := Reactive.Infrastructure.Street.Reference (Street_Mock_Ref);
      SR_Street       : SR_Street_Pkg.Shared_Reference;
      Added : Boolean := FALSE;
   begin
      Street_Mock_Ref.Set_Id (Street_Id);
      SR_Street.Init (SR_Street_Pkg.T_Reference (Street));

      Infrastructure_Registry_Ref.Add_Street (SR_Street, Added);

      Ass.Assert (Infrastructure_Registry_Ref
                  .Contains_Infrastructure (Street_Id),
                  "The Street registry does not "
                  & "contain the Street id");
   end Test_Infrastructure_Contains;

   procedure Test_Infrastructure_Find (T : in out TC.Test_Case'Class)
   is
      Street_Id            : Infra_Id := 488;
      Street_Mock_Ref      : Street_Mock.Reference := Street_Mock.Create;
      Street               : aliased Reactive.Infrastructure.Street.Reference
        := Reactive.Infrastructure.Street.Reference (Street_Mock_Ref);
      SR_Street            : SR_Street_Pkg.Shared_Reference;
      Found_Infrastructure : Reactive.Infrastructure.Reference;
      Added                : Boolean := FALSE;
   begin
      Street_Mock_Ref.Set_Id (Street_Id);
      SR_Street.Init (SR_Street_Pkg.T_Reference (Street));

      Infrastructure_Registry_Ref.Add_Street (SR_Street, Added);

      Found_Infrastructure := Infrastructure_Registry_Ref
         .Find_Infrastructure_By_Id (Street_Id);

      Ass.Assert (
         Found_Infrastructure = Reactive.Infrastructure.Reference (Street),
        "The Infrastructure registry does not contain the Infrastructure");
   end Test_Infrastructure_Find;

   procedure Test_Intersection_Find (T : in out TC.Test_Case'Class)
   is
      Intersection_Id    : Infra_Id := 63;
      Intersection       : aliased Intersection_Mock.Object'Class
         := Intersection_Mock.Create.all;
      Found_Intersection :
         access Reactive.Infrastructure.Intersection.Object'Class;
      Added              : Boolean := FALSE;
   begin
      Intersection.Set_Id (Intersection_Id);

      Infrastructure_Registry_Ref.Add_Intersection (Intersection, Added);

      Found_Intersection := Infrastructure_Registry_Ref
        .Find_Intersection_By_Id (Intersection_Id);

      Ass.Assert (Found_Intersection = Intersection'Access,
                  "The infrastructure registry does not "
                  & "contain the intersection");
   end Test_Intersection_Find;

   procedure Test_Street_Find (T : in out TC.Test_Case'Class)
   is
      Street_Id       : Infra_Id := 64;
      Street_Mock_Ref : Street_Mock.Reference := Street_Mock.Create;
      Street          : aliased Reactive.Infrastructure.Street.Reference
        := Reactive.Infrastructure.Street.Reference (Street_Mock_Ref);
      SR_Street       : SR_Street_Pkg.Shared_Reference;
      Found_Street    : access Reactive.Infrastructure.Street.Object'Class;
      Added           : Boolean := FALSE;
   begin
      Street_Mock_Ref.Set_Id (Street_Id);
      SR_Street.Init (SR_Street_Pkg.T_Reference (Street));

      Infrastructure_Registry_Ref.Add_Street (SR_Street, Added);

      Found_Street := Infrastructure_Registry_Ref
         .Find_Street_By_Id (Street_Id);

      Ass.Assert (Found_Street = Street,
                  "The infrastructure registry does not "
                  & "contain the street");
   end Test_Street_Find;

   procedure Test_Roadway_Find (T : in out TC.Test_Case'Class)
   is
      Roadway_Id : Infra_Id := 65;
      Roadway_Mock_Ref : Roadway_Mock.Reference := Roadway_Mock.Create;
      Roadway : aliased Reactive.Infrastructure.Way.Roadway.Reference
        := Reactive.Infrastructure.Way.Roadway.Reference (Roadway_Mock_Ref);
      Found_Roadway : access Reactive.Infrastructure.Way.Roadway.Object'Class;
      Added : Boolean := FALSE;
   begin
      Roadway_Mock_Ref.Set_Id (Roadway_Id);

      Infrastructure_Registry_Ref.Add_Roadway (Roadway, Added);

      Found_Roadway := Infrastructure_Registry_Ref
        .Find_Roadway_By_Id (Roadway_Id);

      Ass.Assert (Found_Roadway = Roadway,
                  "The infrastructure registry does not "
                  & "contain the road way");
   end Test_Roadway_Find;

   procedure Test_Footway_Find (T : in out TC.Test_Case'Class)
   is
      Footway_Id : Infra_Id := 66;
      Footway_Mock_Ref : Footway_Mock.Reference := Footway_Mock.Create;
      Footway : aliased Reactive.Infrastructure.Way.Footway.Reference
        := Reactive.Infrastructure.Way.Footway.Reference (Footway_Mock_Ref);
      Found_Footway : access Reactive.Infrastructure.Way.Footway.Object'Class;
      Added : Boolean := FALSE;
   begin
      Footway_Mock_Ref.Set_Id (Footway_Id);

      Infrastructure_Registry_Ref.Add_Footway (Footway, Added);

      Found_Footway := Infrastructure_Registry_Ref
        .Find_Footway_By_Id (Footway_Id);

      Ass.Assert (Found_Footway = Footway,
                  "The infrastructure registry does not "
                  & "contain the footway");
   end Test_Footway_Find;

   procedure Test_Bikeway_Find (T : in out TC.Test_Case'Class)
   is
      Bikeway_Id : Infra_Id := 67;
      Bikeway_Mock_Ref : Bikeway_Mock.Reference := Bikeway_Mock.Create;
      Bikeway : aliased Reactive.Infrastructure.Way.Bikeway.Reference
        := Reactive.Infrastructure.Way.Bikeway.Reference (Bikeway_Mock_Ref);
      Found_Bikeway : access Reactive.Infrastructure.Way.Bikeway.Object'Class;
      Added : Boolean := FALSE;
   begin
      Bikeway_Mock_Ref.Set_Id (Bikeway_Id);

      Infrastructure_Registry_Ref.Add_Bikeway (Bikeway, Added);

      Found_Bikeway := Infrastructure_Registry_Ref
        .Find_Bikeway_By_Id (Bikeway_Id);

      Ass.Assert (Found_Bikeway = Bikeway,
                  "The infrastructure registry does not "
                  & "contain the bikeway");
   end Test_Bikeway_Find;

   procedure Test_Lane_Find (T : in out TC.Test_Case'Class)
   is
      Lane_Id : Infra_Id := 63;
      Lane : access Lane_Mock.Object'Class
        := Lane_Mock.Create;
      Lane_Ref : aliased Reactive.Infrastructure.Lane.Reference
         := Reactive.Infrastructure.Lane.Reference (Lane);
      Found_Lane : access Reactive.Infrastructure.Lane.Object'Class;
      Added : Boolean := FALSE;
   begin
      Lane.Set_Id (Lane_Id);

      Infrastructure_Registry_Ref.Add_Lane (Lane_Ref, Added);

      Found_Lane := Infrastructure_Registry_Ref
        .Find_Lane_By_Id (Lane_Id);

      Ass.Assert (Found_Lane = Lane_Ref,
                  "The infrastructure registry does not "
                  & "contain the lane");
   end Test_Lane_Find;

   procedure Test_Stretch_Find (T : in out TC.Test_Case'Class)
   is
      Stretch_Id : Infra_Id := 63;
      Stretch : access Stretch_Mock.Object'Class
        := Stretch_Mock.Create;
      Stretch_Ref : aliased Reactive.Infrastructure.Stretch.Reference
         := Reactive.Infrastructure.Stretch.Reference (Stretch);
      Found_Stretch : access Reactive.Infrastructure.Stretch.Object'Class;
      Added : Boolean := FALSE;
   begin
      Stretch.Set_Id (Stretch_Id);

      Infrastructure_Registry_Ref.Add_Stretch (Stretch_Ref, Added);

      Found_Stretch := Infrastructure_Registry_Ref
        .Find_Stretch_By_Id (Stretch_Id);

      Ass.Assert (Found_Stretch = Stretch,
                  "The infrastructure registry does not "
                  & "contain the stretch");
   end Test_Stretch_Find;

   procedure Test_Remove_All_Infrastructures (T : in out TC.Test_Case'Class)
   is
      Infrastructure1_Id : Infra_Id := 63;
      Infrastructure2_Id : Infra_Id := 64;
      Infrastructure3_Id : Infra_Id := 65;
      Infrastructure1, Infrastructure2, Infrastructure3 : aliased Infrastructure_Mock.Object'Class
        := Infrastructure_Mock.Create.all;
      Added, Removed : Boolean := FALSE;
   begin
      Infrastructure1.Set_Id (Infrastructure1_Id);
      Infrastructure2.Set_Id (Infrastructure2_Id);
      Infrastructure3.Set_Id (Infrastructure3_Id);
   end Test_Remove_All_Infrastructures;

   -----------------------------------------------------
   --                  REGISTRATION
   -----------------------------------------------------
   procedure Register_Tests (T: in out Infrastructure_Registry_Test) is
      use TC.Registration;
   begin
      Register_Routine (Test => T,
                        Routine => Test_Infrastructure_Contains'Access,
                        Name => "Test infrastructure contains");

      Register_Routine (Test => T,
                        Routine => Test_Infrastructure_Find'Access,
                        Name => "Test infrastructure find");

      Register_Routine (Test => T,
                        Routine => Test_Intersection_Find'Access,
                        Name => "Test intersection find");

      Register_Routine (Test => T,
                        Routine => Test_Street_Find'Access,
                        Name => "Test street find");

      Register_Routine (Test => T,
                        Routine => Test_Roadway_Find'Access,
                        Name => "Test roadway find");

      Register_Routine (Test => T,
                        Routine => Test_Footway_Find'Access,
                        Name => "Test footway find");

      Register_Routine (Test => T,
                        Routine => Test_Bikeway_Find'Access,
                        Name => "Test bikeway find");

      Register_Routine (Test => T,
                        Routine => Test_Lane_Find'Access,
                        Name => "Test lane find");

      Register_Routine (Test => T,
                        Routine => Test_Stretch_Find'Access,
                        Name => "Test stretch find");

      Register_Routine (Test => T,
                        Routine => Test_Remove_All_Infrastructures'Access,
                        Name => "Test remove all infrastructure");
   end Register_Tests;

   function Name(T: Infrastructure_Registry_Test) return AU.Message_String is
   begin
      return AU.Format ("Infrastructure_Registry");
   end Name;
end Reactive.Infrastructure_Registry.Tests;
