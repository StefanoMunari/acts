with AUnit.Assertions;
with Ada.Text_IO;

with Active.Agent;
with Active.Traveller.Utils.Mock;

with Reactive.Infrastructure.Street;
with Reactive.Infrastructure.Way.Roadway.Utils.Mock;
with Reactive.Infrastructure.Way.Footway.Utils.Mock;
with Reactive.Infrastructure.Way.Bikeway.Utils.Mock;

with Shared.Direction;

package body Reactive.Infrastructure.Street.Tests is
   package Ass renames AUnit.Assertions;
   package Agent renames Active.Agent;
   package Direction renames Shared.Direction;
   package Roadway_Utils
      renames Reactive.Infrastructure.Way.Roadway.Utils.Mock;
   package Footway_Utils
      renames Reactive.Infrastructure.Way.Footway.Utils.Mock;
   package Bikeway_Utils
      renames Reactive.Infrastructure.Way.Bikeway.Utils.Mock;
   package Traveller_Utils renames Active.Traveller.Utils.Mock;

   -----------------------------------------------------
   --                   SCAFFOLDING
   -----------------------------------------------------
   Street_Id           : Infra_Id;
   Roadway_Id          : Infra_Id;
   Footway_Id          : Infra_Id;
   Bikeway_Id          : Infra_Id;
   Roadway_Lane_Id     : Infra_Id;
   Footway_Lane_Id     : Infra_Id;
   Bikeway_Lane_Id     : Infra_Id;
   Pedestrian_Id       : Agent.Agent_Id;
   Bicycle_Id          : Agent.Agent_Id;
   Bus_Id              : Agent.Agent_Id;
   Street_Orientation  : Direction.Orientation;
   Street_Ref          : Street.Reference;
   Roadway_Utils_Ref   : Roadway_Utils.Reference;
   Footway_Utils_Ref   : Footway_Utils.Reference;
   Bikeway_Utils_Ref   : Bikeway_Utils.Reference;
   Traveller_Utils_Ref : Traveller_Utils.Reference;

   procedure Set_Up (T: in out Street_Test) is
   begin
      Street_Id := 1;
      Roadway_Id := 2;
      Footway_Id := 3;
      Bikeway_Id := 4;
      Roadway_Lane_Id := 5;
      Footway_Lane_Id := 6;
      Bikeway_Lane_Id := 7;
      Pedestrian_Id := Agent.Create_Id_From_Natural (8);
      Bicycle_Id := Agent.Create_Id_From_Natural (9);
      Bus_Id := Agent.Create_Id_From_Natural (10);
      Street_Orientation := Direction.HORIZONTAL;

      Roadway_Utils_Ref := Roadway_Utils.Create;
      Footway_Utils_Ref := Footway_Utils.Create;
      Bikeway_Utils_Ref := Bikeway_Utils.Create;
      Traveller_Utils_Ref := Traveller_Utils.Create;

      Street_Ref
        := Street.Create (Id          => Street_Id,
                          Orientation => Street_Orientation,
                          Roadway_Utils   => Roadway_Utils_Ref,
                          Footway_Utils   => Footway_Utils_Ref,
                          Bikeway_Utils   => Bikeway_Utils_Ref,
                          Traveller_Utils => Traveller_Utils_Ref);
   end Set_Up;

   procedure Set_Roadway (Way_Id : in Infra_Id) is
   begin
      Street_Ref.Roadway_Ids.Append (Way_Id);
   end Set_Roadway;

   procedure Set_Footway (Way_Id : in Infra_Id) is
   begin
      Street_Ref.Footway_Ids.Append (Way_Id);
   end Set_Footway;

   procedure Set_Bikeway (Way_Id : in Infra_Id) is
   begin
      Street_Ref.Bikeway_Ids.Append (Way_Id);
   end Set_Bikeway;

   -----------------------------------------------------
   --                   SCENARIOS
   -----------------------------------------------------
   procedure Test_Find_Lanes_By_Direction (T : in out TC.Test_Case'Class)
   is
      Found_Lanes : Infra_Id_Set.Set;
   begin
      Set_Roadway (Way_Id => Roadway_Id);
      Roadway_Utils_Ref.Set_Return_Value_For_Find_Lane_By_Direction (
         Lane_Id => Roadway_Lane_Id,
         Found => TRUE);

      Set_Footway (Way_Id => Footway_Id);
      Footway_Utils_Ref.Set_Return_Value_For_Find_Lane_By_Direction (
         Lane_Id => Footway_Lane_Id,
         Found => TRUE);

      Set_Bikeway (Way_Id => Bikeway_Id);
      Bikeway_Utils_Ref.Set_Return_Value_For_Find_Lane_By_Direction (
         Lane_Id => Bikeway_Lane_Id,
         Found => TRUE);

      Found_Lanes := Street_Ref.Find_Lanes_By_Direction (
         Travel_Direction => Shared.Direction.NORTH_SOUTH);

      Ass.Assert (Natural(Found_Lanes.Length) = 3,
                  "Lanes found are not exactly 3");

      Ass.Assert (Found_Lanes.Contains (Roadway_Lane_Id),
                  "The id of roadway lane is not returned");

      Ass.Assert (Found_Lanes.Contains (Footway_Lane_Id),
                  "The id of footway lane is not returned");

      Ass.Assert (Found_Lanes.Contains (Bikeway_Lane_Id),
                 "The id of bikeway lane is not returned");
   end Test_Find_Lanes_By_Direction;

   procedure Test_Street_Id (T: in out TC.Test_Case'Class) is
   begin
      Ass.Assert (
         Street_Ref.Id = Street_Id,
         "The id is not equal to value passed at construction");

      Ass.Assert (
         Street_Ref.Get_Id = Street_Id,
         "The id returned by Street's getter is wrong");
   end Test_Street_Id;

   procedure Test_Street_Orientation (T: in out TC.Test_Case'Class) is
   begin
      Ass.Assert (
         Direction."=" (Street_Ref.Orientation, Street_Orientation),
         "The orientation is not equal to value passed at construction");

      Ass.Assert (
         Direction."=" (Street_Ref.Get_Orientation, Street_Orientation),
         "The orientation returned by Street's getter is wrong");
   end Test_Street_Orientation;

   procedure Test_Roadway_Setter (T: in out TC.Test_Case'Class)
   is
      Roadway_Id : Infra_Id := 2;
   begin
      Ass.Assert (Street_Ref.Roadway_Ids.Is_Empty,
                  "A connection with a roadway already exists");

      Street_Ref.Add_Roadway (Roadway_Id);

      Ass.Assert (not Street_Ref.Roadway_Ids.Is_Empty,
                  "A connection with a roadway not already exists");

      Ass.Assert (
         Street_Ref.Roadway_Ids.Contains (Roadway_Id),
        "A connection with a roadway with the specified id not exists");
   end Test_Roadway_Setter;

   procedure Test_Bikeway_Setter (T: in out TC.Test_Case'Class)
   is
      Bikeway_Id : Infra_Id := 2;
   begin
      Ass.Assert (Street_Ref.Bikeway_Ids.Is_Empty,
                  "A connection with a bikeway already exists");

      Street_Ref.Add_Bikeway (Bikeway_Id);

      Ass.Assert (not Street_Ref.Bikeway_Ids.Is_Empty,
                  "A connection with a bikeway not already exists");

      Ass.Assert (
         Street_Ref.Bikeway_Ids.Contains (Bikeway_Id),
        "A connection with a bikeway with the specified id not exists");
   end Test_Bikeway_Setter;

   procedure Test_Footway_Setter (T: in out TC.Test_Case'Class)
   is
      Footway_Id : Infra_Id := 2;
   begin
      Ass.Assert (Street_Ref.Footway_Ids.Is_Empty,
                  "A connection with a footway already exists");

      Street_Ref.Add_Footway (Footway_Id);

      Ass.Assert (not Street_Ref.Footway_Ids.Is_Empty,
                  "A connection with a footway not already exists");

      Ass.Assert (
         Street_Ref.Footway_Ids.Contains (Footway_Id),
        "A connection with a footway with the specified id not exists");
   end Test_Footway_Setter;

   procedure Test_Find_Street (T: in out TC.Test_Case'Class) is
   begin
      Ass.Assert (
                  (Street_Ref.Id = Street_Id),
                  "The street id is not equal to id");

      Ass.Assert (
                  (Street_Ref.Find_Street = Street_Id),
                  "The id returned by Street's finder is wrong");
   end Test_Find_Street;

   procedure Test_Horizontal_Street_Is_Treadable_Towards_East
     (T: in out TC.Test_Case'Class) is
   begin
      Street_Ref.Orientation := Direction.HORIZONTAL;

      Ass.Assert (
                  (Street_Ref.Is_Treadable_In_Direction (Direction.EAST)),
                  "An horizontal street is not treadable towards east");

      Ass.Assert (
                  (not Street_Ref.Is_Not_Treadable_In_Direction (Direction.EAST)),
                  "An horizontal street is not treadable towards east");
   end Test_Horizontal_Street_Is_Treadable_Towards_East;

   procedure Test_Horizontal_Street_Is_Treadable_Towards_West
     (T: in out TC.Test_Case'Class) is
   begin
      Street_Ref.Orientation := Direction.HORIZONTAL;

      Ass.Assert (
                  (Street_Ref.Is_Treadable_In_Direction (Direction.EAST)),
                  "An horizontal  street is not treadable towards west");

      Ass.Assert (
                  (not Street_Ref.Is_Not_Treadable_In_Direction (Direction.EAST)),
                  "An horizontal  street is not treadable towards west");
   end Test_Horizontal_Street_Is_Treadable_Towards_West;

   procedure Test_Horizontal_Street_Is_Not_Treadable_Towards_North
     (T: in out TC.Test_Case'Class) is
   begin
      Street_Ref.Orientation := Direction.HORIZONTAL;

      Ass.Assert (
                  (not Street_Ref.Is_Treadable_In_Direction (Direction.NORTH)),
                  "An horizontal street is treadable towards north");

      Ass.Assert (
                  (Street_Ref.Is_Not_Treadable_In_Direction (Direction.NORTH)),
                  "An horizontal street is treadable towards north");
   end Test_Horizontal_Street_Is_Not_Treadable_Towards_North;

   procedure Test_Horizontal_Street_Is_Not_Treadable_Towards_South
     (T: in out TC.Test_Case'Class) is
   begin
      Street_Ref.Orientation := Direction.HORIZONTAL;

      Ass.Assert (
                  (not Street_Ref.Is_Treadable_In_Direction (Direction.SOUTH)),
                  "An horizontal  street is treadable towards south");

      Ass.Assert (
                  (Street_Ref.Is_Not_Treadable_In_Direction (Direction.SOUTH)),
                  "An horizontal  street is treadable towards south");
   end Test_Horizontal_Street_Is_Not_Treadable_Towards_South;

   procedure Test_Vertical_Street_Is_Treadable_Towards_North
     (T: in out TC.Test_Case'Class) is
   begin
      Street_Ref.Orientation := Direction.VERTICAL;

      Ass.Assert (
                  (Street_Ref.Is_Treadable_In_Direction (Direction.NORTH)),
                  "A vertical street is not treadable towards north");

      Ass.Assert (
                  (not Street_Ref.Is_Not_Treadable_In_Direction (Direction.NORTH)),
                  "A vertical street is not treadable towards north");
   end Test_Vertical_Street_Is_Treadable_Towards_North;

   procedure Test_Vertical_Street_Is_Treadable_Towards_South
     (T: in out TC.Test_Case'Class) is
   begin
      Street_Ref.Orientation := Direction.VERTICAL;

      Ass.Assert (
                  (Street_Ref.Is_Treadable_In_Direction (Direction.SOUTH)),
                  "A vertical street is not treadable towards south");

      Ass.Assert (
                  (not Street_Ref.Is_Not_Treadable_In_Direction (Direction.SOUTH)),
                  "A vertical street is not treadable towards south");
   end Test_Vertical_Street_Is_Treadable_Towards_South;

   procedure Test_Vertical_Street_Is_Not_Treadable_Towards_East
     (T: in out TC.Test_Case'Class) is
   begin
      Street_Ref.Orientation := Direction.VERTICAL;

      Ass.Assert (
                  (not Street_Ref.Is_Treadable_In_Direction (Direction.EAST)),
                  "A vertical street is treadable towards east");

      Ass.Assert (
                  (Street_Ref.Is_Not_Treadable_In_Direction (Direction.EAST)),
                  "A vertical street is treadable towards east");
   end Test_Vertical_Street_Is_Not_Treadable_Towards_East;

   procedure Test_Vertical_Street_Is_Not_Treadable_Towards_West
     (T: in out TC.Test_Case'Class) is
   begin
      Street_Ref.Orientation := Direction.VERTICAL;

      Ass.Assert (
                  (not Street_Ref.Is_Treadable_In_Direction (Direction.EAST)),
                  "A vertical street is treadable towards west");

      Ass.Assert (
                  (Street_Ref.Is_Not_Treadable_In_Direction (Direction.EAST)),
                  "A vertical street is treadable towards west");
   end Test_Vertical_Street_Is_Not_Treadable_Towards_West;

   procedure Test_Equality_Of_Two_Streets (T : in out TC.Test_Case'Class)
   is
      Street_Ref_Copy : Street.Object'Class := Street_Ref.all;
   begin
      Ass.Assert (Street_Ref."="(Street_Ref_Copy),
                  "The streets are not equal");
   end Test_Equality_Of_Two_Streets;

   procedure Test_Inequality_Of_Two_Streets (T : in out TC.Test_Case'Class)
   is
      Street2    : Street.Object'Class := Street_Ref.all;
      Street2_Id : Infra_Id := Street_Id + 1;
   begin
      Street2.Id := Street2_Id;

      Ass.Assert (not Street_Ref."="(Street2),
                  "The streets are equal");
   end Test_Inequality_Of_Two_Streets;

   -----------------------------------------------------
   --                  REGISTRATION
   -----------------------------------------------------
   procedure Register_Tests (T: in out Street_Test) is
      use TC.Registration;
   begin
      Register_Routine (
         Test    => T,
         Routine => Test_Find_Lanes_By_Direction'Access,
         Name    => "Test lane finder by direction");

      Register_Routine (
         Test    => T,
         Routine => Test_Street_Id'Access,
         Name    => "Test street id");

      Register_Routine (
         Test    => T,
         Routine => Test_Street_Orientation'Access,
         Name    => "Test street orientation");

      Register_Routine (
         Test    => T,
         Routine => Test_Roadway_Setter'Access,
         Name    => "Test roadway setter");

      Register_Routine (
         Test    => T,
         Routine => Test_Bikeway_Setter'Access,
         Name    => "Test bikeway setter");

      Register_Routine (
         Test    => T,
         Routine => Test_Footway_Setter'Access,
         Name    => "Test footway setter");

      Register_Routine (
         Test    => T,
         Routine => Test_Find_Street'Access,
         Name    => "Test if Street's finder returns its id");

      Register_Routine (
         Test    => T,
         Routine => Test_Horizontal_Street_Is_Treadable_Towards_East'Access,
         Name    => "Test an horizontal street is treadable towards east");

      Register_Routine (
         Test    => T,
         Routine => Test_Horizontal_Street_Is_Treadable_Towards_West'Access,
         Name    => "Test an horizontal street is treadable towards west");

      Register_Routine (
         Test    => T,
         Routine =>
            Test_Horizontal_Street_Is_Not_Treadable_Towards_North'Access,
         Name    =>
            "Test an horizontal street is not treadable towards north");

      Register_Routine (
         Test    => T,
         Routine =>
            Test_Horizontal_Street_Is_Not_Treadable_Towards_South'Access,
         Name    =>
            "Test an horizontal street is not treadable towards south");

      Register_Routine (
         Test    => T,
         Routine => Test_Vertical_Street_Is_Treadable_Towards_North'Access,
         Name    => "Test a vertical street is treadable towards north");

      Register_Routine (
         Test    => T,
         Routine => Test_Vertical_Street_Is_Treadable_Towards_South'Access,
         Name    => "Test a vertical street is treadable towards south");

      Register_Routine (
         Test    => T,
         Routine => Test_Vertical_Street_Is_Not_Treadable_Towards_East'Access,
         Name    => "Test a vertical street is not treadable towards east");

      Register_Routine (
         Test    => T,
         Routine => Test_Vertical_Street_Is_Not_Treadable_Towards_West'Access,
         Name    => "Test a vertical street is not treadable towards west");

      Register_Routine (
         Test    => T,
         Routine => Test_Equality_Of_Two_Streets'Access,
         Name    => "Test the equality of two streets");

      Register_Routine (
         Test    => T,
         Routine => Test_Inequality_Of_Two_Streets'Access,
         Name    => "Test the inquality of two streets");

   end Register_Tests;

   function Name(T: Street_Test) return AU.Message_String is
   begin
      return AU.Format ("Street");
   end Name;
end Reactive.Infrastructure.Street.Tests;
