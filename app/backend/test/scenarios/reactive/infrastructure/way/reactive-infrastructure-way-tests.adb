with AUnit.Assertions;
with Ada.Text_IO;

with Reactive.Infrastructure.Way.Roadway;
with Active.Traveller.Utils.Mock;

package body Reactive.Infrastructure.Way.Tests is
   package Ass renames AUnit.Assertions;

   -----------------------------------------------------
   --                   SCAFFOLDING
   -----------------------------------------------------
   procedure Set_Up (T: in out Way_Test) is
   begin
      Ass.Assert ((T.Way.Direct_Lane_Existence = FALSE),
                  "The direct lane exists");

      Ass.Assert ((T.Way.Inverse_Lane_Existence = FALSE),
                  "The inverse lane exists");
   end Set_Up;

   -----------------------------------------------------
   --                   SCENARIOS
   -----------------------------------------------------

   procedure Test_Find_Street (T: in out Way_Test)
   is
      Street_Id : Infra_Id := 47;
   begin
      T.Way.Street_Id := Street_Id;

      Ass.Assert ((T.Way.Find_Street = Street_Id),
                  "The id returned by Street's finder is wrong");
   end Test_Find_Street;

   procedure Test_Way_Id_Getter (T: in out Way_Test) is
   begin
      Ass.Assert ((T.Way.Get_Id = T.Way.Id),
                  "The id returned by Way's getter is wrong");
   end Test_Way_Id_Getter;

   procedure Add_Lane (T: in out Way_Test;
                        Lane_Id : in Infra_Id;
                        Lane_Direction : in Direction.Straight;
                        Street_Orientation : in Direction.Orientation;
                        Added : out Boolean) is
   begin
      T.Street_Utils.Set_Return_Value_For_Get_Orientation (Street_Orientation);
      T.Lane_Utils.Set_Return_Value_For_Get_Direction (Lane_Id   => Lane_Id,
                                                       Direction => Lane_Direction);

      T.Way.Add_Lane (Lane_Id => Lane_Id,
                      Added => Added);
   end Add_Lane;

   procedure Test_Direct_Lane_Adding (T: in out Way_Test)
   is
      Lane_Id : Infra_Id := 5;
      Added : Boolean;
   begin
      T.Add_Lane (Lane_Id => Lane_Id,
                  Lane_Direction => Direction.EAST_WEST,
                  Street_Orientation => Direction.HORIZONTAL,
                  Added => Added);

      Ass.Assert ((Added),
                  "New lane is not added to Way");

      Ass.Assert ((T.Way.Direct_Lane_Existence = TRUE),
                  "The direct lane do not exists");

      Ass.Assert ((T.Way.Inverse_Lane_Existence = FALSE),
                  "The inverse lane exists");

      Ass.Assert ((T.Way.Direct_Lane = Lane_Id),
                  "The direct lane is different from the new one");
   end Test_Direct_Lane_Adding;

   procedure Test_Inverse_Lane_Adding (T: in out Way_Test)
   is
      Direct_Lane_Id  : Infra_Id := 5;
      Inverse_Lane_Id : Infra_Id := 12;
      Added : Boolean;
   begin
      T.Add_Lane (Lane_Id => Direct_Lane_Id,
                  Lane_Direction => Direction.EAST_WEST,
                  Street_Orientation => Direction.HORIZONTAL,
                  Added => Added);

      Ass.Assert ((Added),
                  "New lane is not added to Way");

      Ass.Assert ((T.Way.Direct_Lane_Existence = TRUE),
                  "The direct lane do not exists");

      Ass.Assert ((T.Way.Inverse_Lane_Existence = FALSE),
                  "The inverse lane exists");

      Ass.Assert ((T.Way.Direct_Lane = Direct_Lane_Id),
                  "The direct lane is different from the new one");

      T.Add_Lane (Lane_Id => Inverse_Lane_Id,
                  Lane_Direction => Direction.WEST_EAST,
                  Street_Orientation => Direction.HORIZONTAL,
                  Added => Added);

      Ass.Assert ((Added),
                  "New lane is not added to Way");

      Ass.Assert ((T.Way.Direct_Lane_Existence = TRUE),
                  "The direct lane do not exists");

      Ass.Assert ((T.Way.Inverse_Lane_Existence = TRUE),
                  "The inverse lane do not exists");

      Ass.Assert ((T.Way.Inverse_Lane = Inverse_Lane_Id),
                  "The direct lane is different from the new one");
   end Test_Inverse_Lane_Adding;

   procedure Test_Not_Find_Lane_By_Direction_When_There_Are_No_Lanes
     (T: in out Way_Test)
   is
      Found_Lane_Id : Infra_Id;
      Lane_Found : Boolean;
      Lane_Direction : Direction.Straight := Direction.EAST_WEST;
   begin
      T.Way.Find_Lane_By_Direction (Travel_Direction => Lane_Direction,
                                    Lane_Id => Found_Lane_Id,
                                    Found => Lane_Found);

      Ass.Assert (not Lane_Found,
                  "A lane is found");
   end Test_Not_Find_Lane_By_Direction_When_There_Are_No_Lanes;

   procedure Test_Find_Direct_Lane_By_Direction (T: in out Way_Test)
   is
      Direct_Lane_Id : Infra_Id := 5;
      Found_Lane_Id : Infra_Id;
      Added, Lane_Found : Boolean;
      Lane_Direction : Direction.Straight := Direction.EAST_WEST;
   begin
      T.Add_Lane (Lane_Id => Direct_Lane_Id,
                  Lane_Direction => Direction.EAST_WEST,
                  Street_Orientation => Direction.HORIZONTAL,
                  Added => Added);

      Ass.Assert (Added,
                  "New lane is not added to Way");

      Ass.Assert (T.Way.Direct_Lane_Existence = TRUE,
                  "The direct lane do not exists");

      Ass.Assert (T.Way.Inverse_Lane_Existence = FALSE,
                  "The inverse lane exists");

      Ass.Assert (T.Way.Direct_Lane = Direct_Lane_Id,
                  "The direct lane is different from the new one");

      T.Way.Find_Lane_By_Direction (Travel_Direction => Lane_Direction,
                                    Lane_Id => Found_Lane_Id,
                                    Found => Lane_Found);

      Ass.Assert (Lane_Found,
                  "The lane added is not found");

      Ass.Assert (
         Found_Lane_Id = Direct_Lane_Id,
         "The lane id found is different from the last lane added one");
   end Test_Find_Direct_Lane_By_Direction;

   procedure Test_Not_Find_Lane_By_Direction_When_There_Is_One_Lanes (
      T: in out Way_Test)
   is
      Direct_Lane_Id    : Infra_Id := 5;
      Found_Lane_Id     : Infra_Id;
      Added, Lane_Found : Boolean;
      Lane_Direction : Direction.Straight := Direction.WEST_EAST;
   begin
      T.Add_Lane (Lane_Id => Direct_Lane_Id,
                  Lane_Direction => Direction.EAST_WEST,
                  Street_Orientation => Direction.HORIZONTAL,
                  Added => Added);

      Ass.Assert ((Added),
                  "New lane is not added to Way");

      Ass.Assert ((T.Way.Direct_Lane_Existence = TRUE),
                  "The direct lane do not exists");

      Ass.Assert ((T.Way.Inverse_Lane_Existence = FALSE),
                  "The inverse lane exists");

      Ass.Assert ((T.Way.Direct_Lane = Direct_Lane_Id),
                  "The direct lane is different from the new one");

      T.Way.Find_Lane_By_Direction (Travel_Direction => Lane_Direction,
                                    Lane_Id => Found_Lane_Id,
                                    Found => Lane_Found);

      Ass.Assert ((not Lane_Found),
                  "A lane is found");
   end Test_Not_Find_Lane_By_Direction_When_There_Is_One_Lanes;

   procedure Test_Find_Inverse_Lane_By_Direction (T: in out Way_Test)
   is
      Direct_Lane_Id    : Infra_Id := 5;
      Inverse_Lane_Id   : Infra_Id := 12;
      Found_Lane_Id     : Infra_Id;
      Added, Lane_Found : Boolean;
      Inverse_Lane_Direction : Direction.Straight := Direction.WEST_EAST;
   begin
      T.Add_Lane (Lane_Id => Direct_Lane_Id,
                  Lane_Direction => Direction.EAST_WEST,
                  Street_Orientation => Direction.HORIZONTAL,
                  Added => Added);

      Ass.Assert ((Added),
                  "New lane is not added to Way");

      Ass.Assert ((T.Way.Direct_Lane_Existence = TRUE),
                  "The direct lane do not exists");

      Ass.Assert ((T.Way.Inverse_Lane_Existence = FALSE),
                  "The inverse lane exists");

      Ass.Assert ((T.Way.Direct_Lane = Direct_Lane_Id),
                  "The direct lane is different from the new one");

      T.Add_Lane (Lane_Id => Inverse_Lane_Id,
                  Lane_Direction => Inverse_Lane_Direction,
                  Street_Orientation => Direction.HORIZONTAL,
                  Added => Added);

      Ass.Assert ((Added),
                  "New lane is not added to Way");

      Ass.Assert ((T.Way.Direct_Lane_Existence = TRUE),
                  "The direct lane do not exists");

      Ass.Assert ((T.Way.Inverse_Lane_Existence = TRUE),
                  "The inverse lane do not exists");

      Ass.Assert ((T.Way.Inverse_Lane = Inverse_Lane_Id),
                  "The direct lane is different from the new one");

      T.Way.Find_Lane_By_Direction (
         Travel_Direction => Inverse_Lane_Direction,
         Lane_Id          => Found_Lane_Id,
         Found            => Lane_Found);

      Ass.Assert ((Lane_Found),
                  "The lane added is not found");

      Ass.Assert (
         (Found_Lane_Id = Inverse_Lane_Id),
         "The lane id found is different from the last lane added one");
   end Test_Find_Inverse_Lane_By_Direction;

   procedure Test_Street_Setter (T: in out Way_Test)
   is
      Street_Id : Infra_Id := 45;
   begin
      T.Way.Set_Street (Street_Id);

      Ass.Assert ((T.Way.Street_Id = Street_Id),
                  "The street id is not correctly set");
   end Test_Street_Setter;

   procedure Test_Find_Street_Wrapper (T : in out Way_Test'Class) is
   begin
      Test_Find_Street (T);
   end Test_Find_Street_Wrapper;

   procedure Test_Way_Id_Getter_Wrapper (T : in out Way_Test'Class) is
   begin
      Test_Way_Id_Getter (T);
   end Test_Way_Id_Getter_Wrapper;

   procedure Test_Direct_Lane_Adding_Wrapper (T : in out Way_Test'Class) is
   begin
      Test_Direct_Lane_Adding (T);
   end Test_Direct_Lane_Adding_Wrapper;

   procedure Test_Not_Find_Lane_By_Direction_When_There_Are_No_Lanes_Wrapper (
      T : in out Way_Test'Class)
   is
   begin
      Test_Not_Find_Lane_By_Direction_When_There_Are_No_Lanes (T);
   end Test_Not_Find_Lane_By_Direction_When_There_Are_No_Lanes_Wrapper;

   procedure Test_Find_Direct_Lane_By_Direction_Wrapper (
      T : in out Way_Test'Class)
   is
   begin
      Test_Find_Direct_Lane_By_Direction (T);
   end Test_Find_Direct_Lane_By_Direction_Wrapper;

   procedure Test_Not_Find_Lane_By_Direction_When_There_Is_One_Lanes_Wrapper (
      T : in out Way_Test'Class)
   is
   begin
      Test_Not_Find_Lane_By_Direction_When_There_Is_One_Lanes (T);
   end Test_Not_Find_Lane_By_Direction_When_There_Is_One_Lanes_Wrapper;

   procedure Test_Find_Inverse_Lane_By_Direction_Wrapper (
      T : in out Way_Test'Class)
   is
   begin
      Test_Find_Inverse_Lane_By_Direction (T);
   end Test_Find_Inverse_Lane_By_Direction_Wrapper;

   procedure Test_Inverse_Lane_Adding_Wrapper (T : in out Way_Test'Class) is
   begin
      Test_Inverse_Lane_Adding (T);
   end Test_Inverse_Lane_Adding_Wrapper;

   procedure Test_Street_Setter_Wrapper (T : in out Way_Test'Class) is
   begin
      Test_Street_Setter (T);
   end Test_Street_Setter_Wrapper;

    -----------------------------------------------------
   --                  REGISTRATION
   -----------------------------------------------------
   procedure Register_Tests (T: in out Way_Test) is
      package Register_Specific is
         new TC.Specific_Test_Case_Registration (Way_Test);
      use Register_Specific;
   begin

      Register_Wrapper (
         Test    => T,
         Routine => Test_Find_Street_Wrapper'Access,
         Name    =>
            "Test street's finder returns the id of the street associated to"
            & " the way");

      Register_Wrapper (
         Test    => T,
         Routine => Test_Way_Id_Getter_Wrapper'Access,
         Name    => "Test way id getter");

      Register_Wrapper (
         Test    => T,
         Routine =>
            Test_Not_Find_Lane_By_Direction_When_There_Are_No_Lanes_Wrapper'Access,
         Name    => "Test not find lane by direction when there are no lanes");

      Register_Wrapper (
         Test    => T,
         Routine => Test_Direct_Lane_Adding_Wrapper'Access,
         Name    => "Test direct lane adding");

      Register_Wrapper (
         Test    => T,
         Routine =>
            Test_Not_Find_Lane_By_Direction_When_There_Is_One_Lanes_Wrapper'Access,
         Name    => "Test not find lane by direction when there is one lane");

      Register_Wrapper (
         Test    => T,
         Routine => Test_Inverse_Lane_Adding_Wrapper'Access,
         Name    => "Test inverse lane adding");

      Register_Wrapper (
         Test    => T,
         Routine => Test_Find_Direct_Lane_By_Direction_Wrapper'Access,
         Name    => "Test find direct lane by direction");

      Register_Wrapper (
         Test    => T,
         Routine => Test_Find_Inverse_Lane_By_Direction_Wrapper'Access,
         Name    => "Test find inverse lane by direction");

      Register_Wrapper (
         Test    => T,
         Routine => Test_Street_Setter_Wrapper'Access,
         Name    => "Test street setter");

   end Register_Tests;
end Reactive.Infrastructure.Way.Tests;
