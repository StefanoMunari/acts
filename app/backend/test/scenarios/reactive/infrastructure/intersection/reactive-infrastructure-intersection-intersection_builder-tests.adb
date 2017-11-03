with AUnit.Assertions;

with Reactive.Infrastructure.Intersection.Mock;

package body Reactive.Infrastructure.Intersection.Intersection_Builder.Tests is
   package Ass renames AUnit.Assertions;
   package Intersection_Mock renames Reactive.Infrastructure.Intersection.Mock;

   -----------------------------------------------------
   --                   SCENARIOS
   -----------------------------------------------------

   procedure Test_Result_Getter (
      T : in out Intersection_Builder_Test)
   is
      Intersection    : access Intersection_Mock.Object;
   begin
      Intersection := Intersection_Mock.Create;
      Intersection.Set_Id (T.Intersection_Id);
      T.Intersection_Builder.Intersection := Intersection;
      Intersection.Set_Fully_Connected (TRUE);
      Ass.Assert(T.Intersection_Builder.Get_Result = T.Intersection_Id,
                 "Returned intersection is not the expected one");
   end Test_Result_Getter;

   procedure Test_Connection_To_A_Street_Already_Connected_To_Another_Intersection (
      T : in out Intersection_Builder_Test)
   is
      Street_Id        : Infra_Id := 295;
      Stretch_Id       : Infra_Id := 584;
      Stretches        : Infra_Id_List.List := Infra_Id_List.Empty_List;
      Intersection_Id  : Infra_Id := 345;
      Lanes            : Infra_Id_Set.Set;
      Lane_Id          : Infra_Id := 503;
      Street_Direction : Direction.Cardinal := Direction.NORTH;
   begin
      Ass.Assert(not T.Intersection.Is_Connected_At_Direction (Street_Direction),
                 "The intersection has already a street connected "
                 & "at the required direction");

      Stretches.Append (Stretch_Id);
      T.Intersection.Set_Fully_Connected (FALSE);
      T.Street_Utils.Set_Return_Value_For_Is_Not_Treadable_In_Direction (
         Return_Value => FALSE);
      T.Intersection.Set_Return_Value_For_Exists_Street_For_Direction (
         Return_Value => FALSE);
      T.Intersection_Utils.Set_Return_Value_For_Find_Street_Direction (
         Street_Id       => Street_Id,
         Intersection_Id => Intersection_Id,
         Direction       => Shared.Direction.SOUTH,
         Found           => True);
      Lanes.Insert (Lane_Id);
      T.Street_Utils.Set_Return_Value_For_Find_Lanes_By_Direction (
         Return_Value => Lanes);

      T.Intersection_Builder.With_Street (Street_Id => Street_Id,
                                          Stretches => Stretches,
                                          Direction => Street_Direction);

      Ass.Assert(
         T.Intersection.Is_Connected_At_Direction (Street_Direction),
         "The intersection has not already a street connected at the required "
         & "direction");

      Ass.Assert(
         T.Intersection.Get_Street (Street_Direction) = Street_Id,
         "The street is not connected to the intersection");
   end Test_Connection_To_A_Street_Already_Connected_To_Another_Intersection;

   procedure Test_Connection_To_A_Street_Without_Intersections (
      T : in out Intersection_Builder_Test)
   is
      Street_Id        : Infra_Id := 295;
      Stretch_Id       : Infra_Id := 584;
      Stretches        : Infra_Id_List.List := Infra_Id_List.Empty_List;
      Intersections    : Infra_Id_Set.Set;
      Lanes            : Infra_Id_Set.Set;
      Lane_Id          : Infra_Id := 503;
      Street_Direction : Direction.Cardinal := Direction.NORTH;
   begin
      Ass.Assert(not T.Intersection.Is_Connected_At_Direction (Street_Direction),
                 "The intersection has already a street connected "
                 & "at the required direction");

      Stretches.Append (Stretch_Id);
      T.Intersection.Set_Fully_Connected (FALSE);
      T.Street_Utils.Set_Return_Value_For_Is_Not_Treadable_In_Direction (
         Return_Value => FALSE);
      Lanes.Insert (Lane_Id);
      T.Street_Utils.Set_Return_Value_For_Find_Lanes_By_Direction (
         Return_Value => Lanes);

      T.Intersection_Builder.With_Street (Street_Id => Street_Id,
                                          Stretches => Stretches,
                                          Direction => Street_Direction);

      Ass.Assert(T.Intersection.Is_Connected_At_Direction (Street_Direction),
                "There is no street connected with the intersection "
                 & "at the streeet direction");

      Ass.Assert(T.Intersection.Get_Street (Street_Direction) = Street_Id,
                 "The street is not connected to the intersection");
   end Test_Connection_To_A_Street_Without_Intersections;

   procedure Test_Result_Getter_Wrapper (
      T : in out Intersection_Builder_Test'Class) is
   begin
      Test_Result_Getter (T);
   end Test_Result_Getter_Wrapper;

   procedure Test_Connection_To_A_Street_Already_Connected_To_Another_Intersection_Wrapper (
      T : in out Intersection_Builder_Test'Class) is
   begin
      Test_Connection_To_A_Street_Already_Connected_To_Another_Intersection
         (T);
   end Test_Connection_To_A_Street_Already_Connected_To_Another_Intersection_Wrapper;

   procedure Test_Connection_To_A_Street_Without_Intersections_Wrapper (
      T : in out Intersection_Builder_Test'Class) is
   begin
      Test_Connection_To_A_Street_Without_Intersections (T);
   end Test_Connection_To_A_Street_Without_Intersections_Wrapper;

    -----------------------------------------------------
   --                  REGISTRATION
   -----------------------------------------------------
   procedure Register_Tests (T: in out Intersection_Builder_Test) is
      package Register_Specific is
         new TC.Specific_Test_Case_Registration (Intersection_Builder_Test);
      use Register_Specific;
   begin

      Register_Wrapper
        (Test => T,
         Routine => Test_Result_Getter_Wrapper'Access,
         Name => "Test result getter");

      Register_Wrapper
        (Test => T,
         Routine => Test_Connection_To_A_Street_Already_Connected_To_Another_Intersection_Wrapper'Access,
         Name => "Test connection to a street already connected to another"
               & "intersection");

      Register_Wrapper
        (Test => T,
         Routine =>
            Test_Connection_To_A_Street_Without_Intersections_Wrapper'Access,
         Name => "Test connection to a street without intersections");

   end Register_Tests;

end Reactive.Infrastructure.Intersection.Intersection_Builder.Tests;
