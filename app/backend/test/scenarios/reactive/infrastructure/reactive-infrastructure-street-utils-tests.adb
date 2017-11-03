with AUnit.Assertions;
with Ada.Text_IO;

with Active.Agent;

with Reactive.District.Mock;
with Reactive.Infrastructure.Street.Mock;

with Shared.Shared_References_Street;

package body Reactive.Infrastructure.Street.Utils.Tests is
   package Ass renames AUnit.Assertions;
   package Agent renames Active.Agent;
   package District_Mock renames Reactive.District.Mock;
   package Street_Mock renames Reactive.Infrastructure.Street.Mock;
-- shared
   package SR_Street_Pkg renames Shared.Shared_References_Street;

   -----------------------------------------------------
   --                   SCAFFOLDING
   -----------------------------------------------------
   Street_Utils : Reactive.Infrastructure.Street.Utils.Reference;
   District : access District_Mock.Object;

   procedure Set_Up (T: in out Street_Utils_Test) is
   begin
      District := District_Mock.Create;

      Street_Utils
        := Reactive.Infrastructure.Street.Utils
          .Get_Instance (District => District);
   end Set_Up;

   procedure Test_Id_Getter (T: in out TC.Test_Case'Class)
   is
      Street     : access Street_Mock.Object'Class
        := Street_Mock.Create;
      Street_Ref : aliased Reactive.Infrastructure.Street.Reference
         := Reactive.Infrastructure.Street.Reference (Street);
      SR_Street  : SR_Street_Pkg.Shared_Reference;
      Street_Id  : Infra_Id := 345;
      Added      : Boolean := FALSE;
   begin
      Street.Set_Id (Street_Id);
      SR_Street.Init (SR_Street_Pkg.T_Reference (Street_Ref));
      District.Add_Street (SR_Street => SR_Street,
                           Added     => Added);

      Ass.Assert (Added,
                  "The Street is not added to district");

      Ass.Assert (Street_Utils.Get_Id (Street_Id => Street_Id)
                  = Street_Id,
                  "The value returned by Street id getter is wrong");
   end Test_Id_Getter;

   procedure Test_Is_Contained_By (T: in out TC.Test_Case'Class)
   is
      Street : access Street_Mock.Object'Class
        := Street_Mock.Create;
      Street_Ref : aliased Reactive.Infrastructure.Street.Reference
         := Reactive.Infrastructure.Street.Reference (Street);
      SR_Street  : SR_Street_Pkg.Shared_Reference;
      Street_Id : Infra_Id := 345;
      Container_Id : Infra_Id := 1345;
      Added : Boolean := FALSE;
   begin
      Street.Set_Id (Street_Id);
      SR_Street.Init (SR_Street_Pkg.T_Reference (Street_Ref));
      District.Add_Street (SR_Street => SR_Street,
                           Added     => Added);

      Ass.Assert (Added,
                  "The Street is not added to district");

      Street.Set_Return_Value_For_Is_Contained_By (Container_Id, TRUE);

      Ass.Assert (
         Street_Utils.Is_Contained_By (Street_Id    => Street_Id,
                                       Container_Id => Container_Id),
         "The way is not contained by the containter");
   end Test_Is_Contained_By;

   procedure Test_Is_Not_Contained_By (T: in out TC.Test_Case'Class)
   is
      Street : access Street_Mock.Object'Class
        := Street_Mock.Create;
      Street_Ref : aliased Reactive.Infrastructure.Street.Reference
         := Reactive.Infrastructure.Street.Reference (Street);
      SR_Street  : SR_Street_Pkg.Shared_Reference;
      Street_Id : Infra_Id := 345;
      Container_Id : Infra_Id := 1345;
      Added : Boolean := FALSE;
   begin
      Street.Set_Id (Street_Id);
      SR_Street.Init (SR_Street_Pkg.T_Reference (Street_Ref));
      District.Add_Street (SR_Street => SR_Street,
                           Added     => Added);

      Ass.Assert (Added,
                  "The Street is not added to district");

      Street.Set_Return_Value_For_Is_Contained_By (Container_Id, FALSE);

      Ass.Assert (not Street_Utils
                  .Is_Contained_By (Street_Id      => Street_Id,
                                    Container_Id => Container_Id),
                  "The way is contained by the containter");
   end Test_Is_Not_Contained_By;

   procedure Test_Orientation_Getter (T: in out TC.Test_Case'Class)
   is
      Street : access Street_Mock.Object'Class
        := Street_Mock.Create;
      Street_Ref : aliased Reactive.Infrastructure.Street.Reference
         := Reactive.Infrastructure.Street.Reference (Street);
      SR_Street  : SR_Street_Pkg.Shared_Reference;
      Street_Id : Infra_Id := 345;
      Orientation : Direction.Orientation := Shared.Direction.HORIZONTAL;
      Added : Boolean := FALSE;
   begin
      Street.Set_Id (Street_Id);
      SR_Street.Init (SR_Street_Pkg.T_Reference (Street_Ref));
      District.Add_Street (SR_Street => SR_Street,
                           Added     => Added);

      Ass.Assert (Added,
                  "The Street is not added to district");

      Street.Set_Orientation (Orientation);

      Ass.Assert (Direction
                  ."=" (Street_Utils.Get_Orientation (Street_Id => Street_Id),
                    Orientation),
                  "The value returned by Orientation getter is wrong");
   end Test_Orientation_Getter;

   procedure Test_Is_Treadable_In_Direction (T: in out TC.Test_Case'Class)
   is
      Street : access Street_Mock.Object'Class
        := Street_Mock.Create;
      Street_Ref : aliased Reactive.Infrastructure.Street.Reference
         := Reactive.Infrastructure.Street.Reference (Street);
      SR_Street  : SR_Street_Pkg.Shared_Reference;
      Street_Id : Infra_Id := 345;
      Direction : Shared.Direction.Cardinal := Shared.Direction.WEST;
      Added : Boolean := FALSE;
   begin
      Street.Set_Id (Street_Id);
      SR_Street.Init (SR_Street_Pkg.T_Reference (Street_Ref));
      District.Add_Street (SR_Street => SR_Street,
                           Added     => Added);

      Ass.Assert (Added,
                  "The Street is not added to district");

      Street.Set_Return_Value_For_Is_Not_Treadable_In_Direction (FALSE);

      Ass.Assert (not Street_Utils.Is_Not_Treadable_In_Direction
                  (Street_Id => Street_Id,
                   Direction => Direction),
                  "The street is not treadable in a specific direction");
   end Test_Is_Treadable_In_Direction;

   procedure Test_Is_Not_Treadable_In_Direction (T: in out TC.Test_Case'Class)
   is
      Street : access Street_Mock.Object'Class
        := Street_Mock.Create;
      Street_Ref : aliased Reactive.Infrastructure.Street.Reference
         := Reactive.Infrastructure.Street.Reference (Street);
      SR_Street  : SR_Street_Pkg.Shared_Reference;
      Street_Id : Infra_Id := 345;
      Direction : Shared.Direction.Cardinal := Shared.Direction.WEST;
      Added : Boolean := FALSE;
   begin
      Street.Set_Id (Street_Id);
      SR_Street.Init (SR_Street_Pkg.T_Reference (Street_Ref));
      District.Add_Street (SR_Street => SR_Street,
                           Added     => Added);

      Ass.Assert (Added,
                  "The Street is not added to district");

      Street.Set_Return_Value_For_Is_Not_Treadable_In_Direction (TRUE);

      Ass.Assert (Street_Utils.Is_Not_Treadable_In_Direction
                  (Street_Id => Street_Id,
                   Direction => Direction),
                  "The street is treadable in a specific direction");
   end Test_Is_Not_Treadable_In_Direction;

   procedure Test_Lanes_By_Direction_Finder (T: in out TC.Test_Case'Class)
   is
      Street : access Street_Mock.Object'Class
        := Street_Mock.Create;
      Street_Ref : aliased Reactive.Infrastructure.Street.Reference
         := Reactive.Infrastructure.Street.Reference (Street);
      SR_Street  : SR_Street_Pkg.Shared_Reference;
      Street_Id : Infra_Id := 345;
      Direction : Shared.Direction.Straight := Shared.Direction.WEST_EAST;
      Added : Boolean := FALSE;
      Lanes : Infra_Id_Set.Set;
   begin
      Lanes.Insert (24);
      Lanes.Insert (43);
      Lanes.Insert (54);

      Street.Set_Id (Street_Id);
      SR_Street.Init (SR_Street_Pkg.T_Reference (Street_Ref));

      District.Add_Street (SR_Street => SR_Street,
                           Added     => Added);

      Ass.Assert (Added,
                  "The Street is not added to district");

      Street.Set_Return_Value_For_Find_Lanes_By_Direction (Lanes);

      Ass.Assert (Street_Utils.Find_Lanes_By_Direction
                  (Street_Id        => Street_Id,
                   Travel_Direction => Direction)."=" (Lanes),
                  "The street is not treadable in a specific direction");
   end Test_Lanes_By_Direction_Finder;

   -----------------------------------------------------
   --                  REGISTRATION
   -----------------------------------------------------
   procedure Register_Tests (T: in out Street_Utils_Test) is
      use TC.Registration;
   begin
      Register_Routine (Test    => T,
                        Routine => Test_Id_Getter'Access,
                        Name    => "Test id getter");

      Register_Routine (Test => T,
                        Routine => Test_Is_Contained_By'Access,
                        Name => "Test way is contained by a container");

      Register_Routine (Test => T,
                        Routine => Test_Is_Not_Contained_By'Access,
                        Name => "Test way is not contained by a container");
--
      Register_Routine (Test => T,
                        Routine => Test_Orientation_Getter'Access,
                        Name => "Test orientation getter");

      Register_Routine (Test    => T,
                        Routine => Test_Is_Treadable_In_Direction'Access,
                        Name    => "Test is treadable in direction");

      Register_Routine (Test    => T,
                        Routine => Test_Is_Not_Treadable_In_Direction'Access,
                        Name    => "Test is not treadable in direction");

      Register_Routine (Test    => T,
                        Routine => Test_Lanes_By_Direction_Finder'Access,
                        Name    => "Test lanes by direction finder");
   end Register_Tests;

   function Name(T: Street_Utils_Test) return AU.Message_String is
   begin
      return AU.Format ("Street_Utils");
   end Name;
end Reactive.Infrastructure.Street.Utils.Tests;
