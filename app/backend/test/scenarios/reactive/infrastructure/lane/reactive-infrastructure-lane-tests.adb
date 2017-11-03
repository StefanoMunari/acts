with AUnit.Assertions;
with Ada.Text_IO;

with Active.Traveller.Mock;

with Interface_Layer.Remote.Stub;
with Interface_Layer.Remote.Stub.Mock;
with Interface_Layer.Service.Pipelines.Handler.Abstract_Factory;
with Interface_Layer.Service.Pipelines.Handler.Mock_Factory;
with Interface_Layer.Wrappers.Application.Abstract_Factory;
with Interface_Layer.Wrappers.Application.Mock_Factory;

with Reactive.District;
with Reactive.Infrastructure_Registry.Mock;
with Reactive.Traveller_Registry.Mock;

package body Reactive.Infrastructure.Lane.Tests is

   package Ass renames AUnit.Assertions;
   package H_Abstract_Factory
      renames Interface_Layer.Service.Pipelines.Handler.Abstract_Factory;
   package H_Mock_Factory
      renames Interface_Layer.Service.Pipelines.Handler.Mock_Factory;
   package W_Abstract_Factory
      renames Interface_Layer.Wrappers.Application.Abstract_Factory;
   package W_Mock_Factory
      renames Interface_Layer.Wrappers.Application.Mock_Factory;
   package Stub renames Interface_Layer.Remote.Stub;
   package Stub_Mock renames Interface_Layer.Remote.Stub.Mock;
   package I_Registry_Mock renames Reactive.Infrastructure_Registry.Mock;
   package T_Registry_Mock renames Reactive.Traveller_Registry.Mock;
   package Traveller_Pkg  renames Active.Traveller;
   package Traveller_Mock renames Active.Traveller.Mock;
   package District renames Reactive.District;

   Traveller : aliased Traveller_Mock.Object'Class
        := Traveller_Mock.Create.all;
   Traveller_Direction : Direction.Straight := Direction.SOUTH_NORTH;
   Traveller_Id        : Agent.Agent_Id := Agent.Create_Id_From_Natural (610);
   Traveller_Speed     : Natural := 5;
   Handler_Factory     : H_Abstract_Factory.Reference;
   Wrapper_Factory     : W_Abstract_Factory.Reference;
   I_Registry          : I_Registry_Mock.Reference;
   T_Registry          : T_Registry_Mock.Reference;
   Stub_Ref            : aliased Stub.Object;
   District_Ref    : Reactive.District.Reference;

   -----------------------------------------------------
   --                   SCAFFOLDING
   -----------------------------------------------------
   procedure Set_Up (T: in out Lane_Test) is
      Traveller_Added : Boolean;
      Traveller_Ref   : Traveller_Pkg.Reference := Traveller'Access;
   begin
      Handler_Factory := new H_Mock_Factory.Object;
      Wrapper_Factory := new W_Mock_Factory.Object;
      I_Registry := I_Registry_Mock.Create;
      T_Registry := T_Registry_Mock.Create;
      Stub_Ref := Stub.Object (Stub_Mock.Create (Handler_Factory));
      District_Ref := District.Get_Instance (
         null, null, App_Wrapper_Factory => Wrapper_Factory,
         Stub => Stub_Ref'access);
      Traveller.Set_Id (Traveller_Id);
      Traveller.Set_Current_Speed (Traveller_Speed);
      if not District_Ref.Contains_Traveller (Traveller_Id) then
         District_Ref.Add_Traveller (Traveller_Ref, Traveller_Added);
      end if;
   end Set_Up;

   procedure Tear_Down (T: in out Lane_Test) is
      Traveller_Removed : Boolean;
   begin
      if District_Ref.Contains_Traveller (Traveller_Id) then
         District_Ref.Remove_Traveller (Traveller_Id, Traveller_Removed);
      end if;
   end Tear_Down;

   -----------------------------------------------------
   --                   SCENARIOS
   -----------------------------------------------------

   procedure Test_Find_Street (T : in out Lane_Test)
   is
      Street_Id : Infra_Id := 47;
   begin
      T.Way_Utils.Set_Return_Value_For_Find_Street (Street_Id);

      Ass.Assert ((T.Lane.Find_Street = Street_Id),
                  "The id returned by Street's finder is wrong");
   end Test_Find_Street;

   procedure Set_Intersection (T: in out Lane_Test;
                               Intersection_Id : in Infra_Id) is
   begin
      T.Lane.Intersection_Id := Intersection_Id;
      T.Lane.Intersection_Existence := TRUE;
   end Set_Intersection;

   procedure Test_Intersections_Finder (T : in out Lane_Test)
   is
      Intersection_Id : Infra_Id := 256;
   begin
      T.Set_Intersection (Intersection_Id => Intersection_Id);

      Ass.Assert
        ((Natural (T.Lane.Find_Intersections.Length) = 1),
         "The intersections found are different from specified ones.");

      Ass.Assert
        ((not T.Lane.Find_Intersections.Is_Empty
            and T.Lane.Find_Intersections.First_Element = Intersection_Id),
         "The intersections found are different from specified ones.");
   end Test_Intersections_Finder;

   procedure Test_Lane_Id_Getter (T: in out Lane_Test) is
   begin
      Ass.Assert ((T.Lane.Get_Id = T.Lane.Id),
                  "The id returned by Lane's getter is wrong");
   end Test_Lane_Id_Getter;

   procedure Test_Lane_Direction_Getter (T: in out Lane_Test) is
   begin
      Ass.Assert ((Direction."="(T.Lane.Get_Direction, T.Lane.Direction)),
                  "The direction returned by Lane's getter is wrong");
   end Test_Lane_Direction_Getter;

   procedure Append_Stretch (T          : in out Lane_Test;
                             Stretch_Id : in Infra_Id;
                             Added      : out Boolean) is
   begin
      T.Lane.Append_Stretch (Stretch_Id => Stretch_Id,
                             Added      => Added);
   end Append_Stretch;

   procedure Test_Stretch_Adding (T : in out Lane_Test)
   is
      Stretch_Id : Infra_Id := 5;
      Added      : Boolean;
   begin
      Ass.Assert ((T.Lane.Protected_Stretches.Count_Stretches = 0),
                  "The lane has already some strech");

      T.Append_Stretch (Stretch_Id => Stretch_Id,
                        Added => Added);

      Ass.Assert ((Added),
                  "New lane is not added to Lane");

      Ass.Assert ((T.Lane.Protected_Stretches.Contains_Stretch (Stretch_Id)),
                  "The stretch is not part of the lane");

      Ass.Assert ((T.Lane.Protected_Stretches.Count_Stretches > 0),
                  "The lane has no streches");

      Ass.Assert ((T.Lane.Protected_Stretches.Count_Stretches <= 1),
                  "The added stretch is not the only one of the lane");
   end Test_Stretch_Adding;

   procedure Test_Way_Setter (T : in out Lane_Test)
   is
      Way_Id : Infra_Id := 45;
   begin
      T.Lane.Set_Way (Way_Id);

      Ass.Assert ((T.Lane.Way_Id = Way_Id),
                  "The way id is not correctly set");
   end Test_Way_Setter;

   procedure Test_Find_Stretch_Position (T : in out Lane_Test)
   is
      Stretch0_Id      : Infra_Id := 23;
      Stretch1_Id      : Infra_Id := 45;
      Stretch2_Id      : Infra_Id := 95;
      Stretch_Position : Natural;
      Added, Found : Boolean;
   begin
      T.Append_Stretch (Stretch_Id => Stretch0_Id,
                        Added => Added);

      T.Append_Stretch (Stretch_Id => Stretch1_Id,
                        Added => Added);

      T.Append_Stretch (Stretch_Id => Stretch2_Id,
                        Added => Added);

      T.Lane.Find_Stretch_Position
        (Stretch_Id => Stretch2_Id,
         Stretch_Position => Stretch_Position,
         Found => Found);

      Ada.Text_IO.Put_Line ("Stretch_Position => "
                            & Natural'Image (Stretch_Position));

      Ass.Assert ((Found = TRUE),
                  "The stretch position is not found");

      Ass.Assert ((Stretch_Position = 2),
                  "The stretch position is wrong");
   end Test_Find_Stretch_Position;

   procedure Test_Not_Find_Stretch_Position (T : in out Lane_Test)
   is
      Stretch0_Id      : Infra_Id := 23;
      Stretch1_Id      : Infra_Id := 45;
      Stretch2_Id      : Infra_Id := 95;
      Stretch3_Id      : Infra_Id := 92;
      Stretch_Position : Natural;
      Added, Found     : Boolean;
   begin
      T.Append_Stretch (Stretch_Id => Stretch0_Id,
                        Added => Added);

      T.Append_Stretch (Stretch_Id => Stretch1_Id,
                        Added => Added);

      T.Append_Stretch (Stretch_Id => Stretch2_Id,
                        Added => Added);

      T.Lane.Find_Stretch_Position
        (Stretch_Id => Stretch3_Id,
         Stretch_Position => Stretch_Position,
         Found => Found);

      Ass.Assert ((Found = FALSE),
                  "The stretch position is found");
   end Test_Not_Find_Stretch_Position;

   procedure Test_Equality_Of_Two_Lanes (T : in out Lane_Test)
   is
      Lane_Copy : Lane.Object'Class := T.Lane.all;
   begin
      Ass.Assert (T.Lane."="(Lane_Copy),
                  "The lanes are not equal");
   end Test_Equality_Of_Two_Lanes;

   procedure Test_Inequality_Of_Two_Lanes (T : in out Lane_Test)
   is
      Lane2    : Lane.Object'Class := T.Lane.all;
      Lane2_Id : Infra_Id := T.Lane.Id + 1;
   begin
      Lane2.Id := Lane2_Id;

      Ass.Assert (not T.Lane."="(Lane2),
                  "The lanes are equal");
   end Test_Inequality_Of_Two_Lanes;

   procedure Test_Stretches_Counter (T : in out Lane_Test)
   is
      Added : Boolean;
   begin
      Ass.Assert (T.Lane.Count_Stretches = 0,
                  "There are already some stretches");

      T.Lane.Protected_Stretches.Append_Stretch (Stretch_Id => 1,
                                                 Added      => Added);
      T.Lane.Protected_Stretches.Append_Stretch (Stretch_Id => 3,
                                                 Added      => Added);
      T.Lane.Protected_Stretches.Append_Stretch (Stretch_Id => 7,
                                                 Added      => Added);

      Ass.Assert (T.Lane.Count_Stretches = 3,
                  "The stretches counter makes some mistake");
   end Test_Stretches_Counter;

   procedure Test_Intersection_Adding (T : in out Lane_Test)
   is
      Intersection_Id : Infra_Id := 34;
      Added           : Boolean  := FALSE;
   begin
      Ass.Assert (not T.Lane.Intersection_Existence,
                  "There is already an intersection added");

      T.Lane.Add_Intersection (Intersection_Id => Intersection_Id,
                               Added           => Added);

      Ass.Assert (T.Lane.Intersection_Existence,
                  "There is not an intersection added");

      Ass.Assert (T.Lane.Intersection_Id = Intersection_Id,
                  "The intersection adder did not add the intersection requested");
   end Test_Intersection_Adding;

   procedure Test_No_Stretch_Next_To_A_Missing_Stretch (T : in out Lane_Test)
   is
      Missing_Stretch : Infra_Id := 1234;
   begin
      Ass.Assert
        (not T.Lane.Protected_Stretches.Contains_Stretch
           (Stretch_Id => Missing_Stretch),
         "Passing stretch is contained into lane");

      Ass.Assert
        (not T.Lane.Protected_Stretches.Has_Next_Stretch
           (Stretch_Id => Missing_Stretch),
         "Passing stretch has a next stretch");
   end Test_No_Stretch_Next_To_A_Missing_Stretch;

   procedure Test_Find_Street_Wrapper (T : in out Lane_Test'Class) is
   begin
      Test_Find_Street (T);
   end Test_Find_Street_Wrapper;

   procedure Test_Intersections_Finder_Wrapper (T : in out Lane_Test'Class) is
   begin
      Test_Intersections_Finder (T);
   end Test_Intersections_Finder_Wrapper;

   procedure Test_Lane_Id_Getter_Wrapper (T : in out Lane_Test'Class) is
   begin
      Test_Lane_Id_Getter (T);
   end Test_Lane_Id_Getter_Wrapper;

   procedure Test_Lane_Direction_Getter_Wrapper (T : in out Lane_Test'Class) is
   begin
      Test_Lane_Direction_Getter (T);
   end Test_Lane_Direction_Getter_Wrapper;

   procedure Test_Stretch_Adding_Wrapper (T : in out Lane_Test'Class) is
   begin
      Test_Stretch_Adding (T);
   end Test_Stretch_Adding_Wrapper;

   procedure Test_Way_Setter_Wrapper (T : in out Lane_Test'Class) is
   begin
      Test_Way_Setter (T);
   end Test_Way_Setter_Wrapper;

   procedure Test_Find_Stretch_Position_Wrapper (T : in out Lane_Test'Class) is
   begin
      Test_Find_Stretch_Position (T);
   end Test_Find_Stretch_Position_Wrapper;

   procedure Test_Not_Find_Stretch_Position_Wrapper (T : in out Lane_Test'Class) is
   begin
      Test_Not_Find_Stretch_Position (T);
   end Test_Not_Find_Stretch_Position_Wrapper;

   procedure Test_Equality_Of_Two_Lanes_Wrapper (T : in out Lane_Test'Class) is
   begin
      Test_Equality_Of_Two_Lanes (T);
   end Test_Equality_Of_Two_Lanes_Wrapper;

   procedure Test_Inequality_Of_Two_Lanes_Wrapper (T : in out Lane_Test'Class) is
   begin
      Test_Inequality_Of_Two_Lanes (T);
   end Test_Inequality_Of_Two_Lanes_Wrapper;

   procedure Test_Stretches_Counter_Wrapper (T : in out Lane_Test'Class) is
   begin
      Test_Stretches_Counter (T);
   end Test_Stretches_Counter_Wrapper;

   procedure Test_Intersection_Adding_Wrapper (T : in out Lane_Test'Class) is
   begin
      Test_Intersection_Adding (T);
   end Test_Intersection_Adding_Wrapper;

   procedure Test_No_Stretch_Next_To_A_Missing_Stretch_Wrapper
     (T : in out Lane_Test'Class) is
   begin
      Test_No_Stretch_Next_To_A_Missing_Stretch (T);
   end Test_No_Stretch_Next_To_A_Missing_Stretch_Wrapper;

    -----------------------------------------------------
   --                  REGISTRATION
   -----------------------------------------------------
   procedure Register_Tests (T: in out Lane_Test) is
      package Register_Specific is
         new TC.Specific_Test_Case_Registration (Lane_Test);
      use Register_Specific;
   begin

      Register_Wrapper
        (Test => T,
         Routine => Test_Find_Street_Wrapper'Access,
         Name => "Test street's finder returns the id of the street associated to the Lane");

      Register_Wrapper
        (Test => T,
         Routine => Test_Intersections_Finder_Wrapper'Access,
         Name => "Test intersections' finder returns the ids of the intersections associated to the Lane");

      Register_Wrapper
        (Test => T,
         Routine => Test_Lane_Id_Getter_Wrapper'Access,
         Name => "Test Lane id getter");

      Register_Wrapper
        (Test => T,
         Routine => Test_Lane_Direction_Getter_Wrapper'Access,
         Name => "Test Lane direction getter");

      Register_Wrapper
        (Test => T,
         Routine => Test_Stretch_Adding_Wrapper'Access,
         Name => "Test stretch adding");

      Register_Wrapper
        (Test => T,
         Routine => Test_Way_Setter_Wrapper'Access,
         Name => "Test way setter");

      Register_Wrapper
        (Test => t,
         Routine => Test_Find_Stretch_Position_Wrapper'Access,
         Name => "Test stretch position calculator");

      Register_Wrapper
        (Test => t,
         Routine => Test_Not_Find_Stretch_Position_Wrapper'Access,
           Name => "Test not find stretch position");

      Register_Wrapper
        (Test => t,
         Routine => Test_Equality_Of_Two_Lanes_Wrapper'Access,
         Name => "Test the equality of two lanes");

      Register_Wrapper
        (Test => T,
         Routine => Test_Inequality_Of_Two_Lanes_Wrapper'Access,
         Name => "Test the inequality of two lanes");

      Register_Wrapper
        (Test => T,
         Routine => Test_Stretches_Counter_Wrapper'Access,
         Name => "Test stretches counter");

         null;
      Register_Wrapper
        (Test => T,
         Routine => Test_Intersection_Adding_Wrapper'Access,
         Name => "Test intersection adding");

      Register_Wrapper
        (Test => T,
         Routine => Test_No_Stretch_Next_To_A_Missing_Stretch_Wrapper'Access,
         Name => "Test no stretch next to a missing stretch");

   end Register_Tests;
end Reactive.Infrastructure.Lane.Tests;
