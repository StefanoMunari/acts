with AUnit.Assertions;
with Ada.Text_IO;

with Active.Agent;

package body Active.Traveller.Tests is
   package Ass renames AUnit.Assertions;
   package Agent renames Active.Agent;
   use Agent;
   use Reactive.Stretch_Type_Package;

   -----------------------------------------------------
   --                   SCENARIOS
   -----------------------------------------------------

   procedure Test_Traveller_Id_Getter (T: in out Traveller_Test) is
   begin
      Ass.Assert ((T.Traveller.Get_Id = T.Traveller.Id),
                  "The id returned by Traveller's getter is wrong");
   end Test_Traveller_Id_Getter;

   procedure Test_Position_Getter (T : in out Traveller_Test) is
   begin
      T.Current_Position := 42;
      T.Traveller.Current_Position := T.Current_Position;
      Ass.Assert (
         T.Traveller.Get_Position = T.Current_Position,
         "The position returned by Traveller's getter is wrong");
   end Test_Position_Getter;

   procedure Test_Position_Setter (T : in out Traveller_Test) is
   begin
      T.Current_Position := 43;
      T.Traveller.Set_Position (T.Current_Position);
      Ass.Assert (
         T.Traveller.Current_Position = T.Current_Position,
         "The position has not been setted correctly on the Traveller");
   end Test_Position_Setter;

   procedure Test_Maximum_Speed_Getter (T : in out Traveller_Test) is
   begin
      Ass.Assert ((T.Traveller.Get_Maximum_Speed = T.Traveller.Maximum_Speed),
                  "The maximum speed returned by Traveller's getter is wrong");
   end Test_Maximum_Speed_Getter;

   procedure Test_Current_Speed_Getter (T : in out Traveller_Test) is
   begin
      Ass.Assert ((T.Traveller.Get_Current_Speed = T.Traveller.Current_Speed),
                  "The speed returned by Traveller's getter is wrong");
   end Test_Current_Speed_Getter;

   procedure Test_Current_Speed_Setter (T : in out Traveller_Test)
   is
      New_Speed : Natural := T.Traveller.Maximum_Speed - 1;
   begin
      T.Traveller.Set_Current_Speed (New_Speed);

      Ass.Assert ((T.Traveller.Current_Speed = New_Speed),
                  "The speed of Traveller is not changed");
   end Test_Current_Speed_Setter;

   procedure Test_Scheduled_For_Getter (T : in out Traveller_Test) is
   begin
      T.Scheduled_For := 42.0;
      T.Traveller.Scheduled_For := T.Scheduled_For;
      Ass.Assert (
         T.Traveller.Get_Scheduled_For = T.Scheduled_For,
         "The scheduled for value returned by Traveller's getter is wrong");
   end Test_Scheduled_For_Getter;

   procedure Test_Scheduled_For_Setter (T : in out Traveller_Test)
   is
   begin
      T.Scheduled_For := 43.0;
      T.Traveller.Set_Scheduled_For (T.Scheduled_For);
      Ass.Assert (
         T.Traveller.Scheduled_For = T.Scheduled_For,
         "The deferral has not been setted correctly on the Traveller");
   end Test_Scheduled_For_Setter;

   procedure Test_Source_Getter (T : in out Traveller_Test)
   is
      Source    : Slice.Map := Slice.Empty_Map;
      Foot_List : Infra_Id_List.List := Infra_Id_List.Empty_List;
      Bike_List : Infra_Id_List.List := Infra_Id_List.Empty_List;
      Road_List : Infra_Id_List.List := Infra_Id_List.Empty_List;
   begin
      Foot_List.Append (700);
      Source.Include (Key => FOOT, New_Item => Foot_List);
      Bike_List.Append (701);
      Bike_List.Append (702);
      Source.Include (Key => BIKE, New_Item => Bike_List);
      Road_List.Append (703);
      Source.Include (Key => FOOT, New_Item => Road_List);

      T.Travel_Ref.Set_Return_Value_For_Get_Route_Source (Source);

      Ass.Assert (
          Slice."=" (T.Traveller.Get_Travel_Source, Source),
         "The source returned by Traveller's getter is wrong");
   end Test_Source_Getter;

   procedure Test_Destination_Getter (T : in out Traveller_Test)
   is
      Destination : Slice.Map := Slice.Empty_Map;
      Foot_List   : Infra_Id_List.List := Infra_Id_List.Empty_List;
      Bike_List   : Infra_Id_List.List := Infra_Id_List.Empty_List;
      Road_List   : Infra_Id_List.List := Infra_Id_List.Empty_List;
   begin
      Foot_List.Append (800);
      Destination.Include (Key => FOOT, New_Item => Foot_List);
      Bike_List.Append (801);
      Bike_List.Append (802);
      Destination.Include (Key => BIKE, New_Item => Bike_List);
      Road_List.Append (803);
      Destination.Include (Key => FOOT, New_Item => Road_List);

      T.Travel_Ref.Set_Return_Value_For_Get_Route_Destination (Destination);

      Ass.Assert (
          Slice."=" (T.Traveller.Get_Travel_Destination, Destination),
         "The destination returned by Traveller's getter is wrong");
   end Test_Destination_Getter;

   procedure Test_Contains_Step_Infra_Id (T : in out Traveller_Test)
   is
      Step_Id_True  : Infra_Id := 55;
      Step_Id_False : Infra_Id := 56;
   begin
      T.Travel_Ref.Set_Return_Value_For_Contains (Step_Id_True, True);
      Ass.Assert (
         T.Traveller.Does_Travel_Contain_Step (Step_Id_True) = True,
         "The travel does not contain the step");

      T.Travel_Ref.Set_Return_Value_For_Contains (Step_Id_False, False);
      Ass.Assert (
         T.Traveller.Does_Travel_Contain_Step (Step_Id_False) = False,
         "The travel contains the step");
   end Test_Contains_Step_Infra_Id;

   procedure Test_Contains_Step_Slice (T : in out Traveller_Test)
   is
      Step_Id_True  : Slice.Map := Slice.Empty_Map;
      Step_Id_False : Slice.Map := Slice.Empty_Map;
   begin
   -- TODO: Change this
      T.Travel_Ref.Set_Return_Value_For_Contains_Slice (True);
      Ass.Assert (
         T.Traveller.Does_Travel_Contain_Steps (Step_Id_True) = True,
         "The travel does not contain the step");

      T.Travel_Ref.Set_Return_Value_For_Contains_Slice (False);
      Ass.Assert (
         T.Traveller.Does_Travel_Contain_Steps (Step_Id_False) = False,
         "The travel contains the step");
   end Test_Contains_Step_Slice;

   procedure Test_Has_Steps_Ahead (T : in out Traveller_Test) is
   begin
      T.Travel_Ref.Set_Return_Value_For_Has_Next_Step (TRUE);

      Ass.Assert ((T.Traveller.Has_Next_Step = TRUE),
                  "The Traveller has no next step");
   end Test_Has_Steps_Ahead;

   procedure Test_Has_No_Step_Ahead (T : in out Traveller_Test) is
   begin
      T.Travel_Ref.Set_Return_Value_For_Has_Next_Step (FALSE);

      Ass.Assert ((T.Traveller.Has_Next_Step = FALSE),
                  "The Traveller has next step");
   end Test_Has_No_Step_Ahead;

   procedure Test_Is_Travelling (T : in out Traveller_Test) is
   begin
      T.Travel_Ref.Set_Return_Value_For_Is_Progressing (TRUE);

      Ass.Assert ((T.Traveller.Is_Travelling = TRUE),
                  "The Traveller has next step");
   end Test_Is_Travelling;

   procedure Test_Is_Not_Travelling (T : in out Traveller_Test) is
   begin
      T.Travel_Ref.Set_Return_Value_For_Is_Progressing (FALSE);

      Ass.Assert ((T.Traveller.Is_Travelling = FALSE),
                  "The Traveller has next step");
   end Test_Is_Not_Travelling;

   procedure Test_Equality_Of_Two_Travellers (T : in out Traveller_Test)
   is
      Traveller_Copy : Traveller.Object'Class := T.Traveller.all;
   begin
      Ass.Assert (T.Traveller."="(Traveller_Copy),
                  "The travellers are not equal");
   end Test_Equality_Of_Two_Travellers;

   -- TODO: Decide whether it is possible to fix this test
   --procedure Test_Inequality_Of_Two_Travellers (T : in out Traveller_Test)
   --is
   --   Traveller2 : Traveller.Object'Class := T.Traveller.all;
   --   Traveller2_Id : Natural := T.Traveller.Id + 1;
   --begin
   --   Traveller2.Id := Traveller2_Id;

   --   Ass.Assert (not T.Traveller."="(Traveller2),
   --               "The travellers are equal");
   --end Test_Inequality_Of_Two_Travellers;

   procedure Test_Traveller_Id_Getter_Wrapper (
      T : in out Traveller_Test'Class) is
   begin
      Test_Traveller_Id_Getter (T);
   end Test_Traveller_Id_Getter_Wrapper;

   procedure Test_Position_Getter_Wrapper (
      T : in out Traveller_Test'Class) is
   begin
      Test_Position_Getter (T);
   end Test_Position_Getter_Wrapper;

   procedure Test_Position_Setter_Wrapper (
      T : in out Traveller_Test'Class) is
   begin
      Test_Position_Setter (T);
   end Test_Position_Setter_Wrapper;

   procedure Test_Maximum_Speed_Getter_Wrapper (
      T : in out Traveller_Test'Class) is
   begin
      Test_Maximum_Speed_Getter (T);
   end Test_Maximum_Speed_Getter_Wrapper;

   procedure Test_Current_Speed_Getter_Wrapper (
      T : in out Traveller_Test'Class) is
   begin
      Test_Current_Speed_Getter (T);
   end Test_Current_Speed_Getter_Wrapper;

   procedure Test_Current_Speed_Setter_Wrapper (
      T : in out Traveller_Test'Class) is
   begin
      Test_Current_Speed_Setter (T);
   end Test_Current_Speed_Setter_Wrapper;

   procedure Test_Scheduled_For_Getter_Wrapper (
      T : in out Traveller_Test'Class) is
   begin
      Test_Scheduled_For_Getter (T);
   end Test_Scheduled_For_Getter_Wrapper;

   procedure Test_Scheduled_For_Setter_Wrapper (
      T : in out Traveller_Test'Class) is
   begin
      Test_Scheduled_For_Setter (T);
   end Test_Scheduled_For_Setter_Wrapper;

   procedure Test_Source_Getter_Wrapper (
      T : in out Traveller_Test'Class) is
   begin
      Test_Source_Getter (T);
   end Test_Source_Getter_Wrapper;

   procedure Test_Destination_Getter_Wrapper (
      T : in out Traveller_Test'Class) is
   begin
      Test_Destination_Getter (T);
   end Test_Destination_Getter_Wrapper;

   procedure Test_Contains_Step_Infra_Id_Wrapper (
      T : in out Traveller_Test'Class) is
   begin
      Test_Contains_Step_Infra_Id (T);
   end Test_Contains_Step_Infra_Id_Wrapper;

   procedure Test_Contains_Step_Slice_Wrapper (
      T : in out Traveller_Test'Class) is
   begin
      Test_Contains_Step_Slice (T);
   end Test_Contains_Step_Slice_Wrapper;

   procedure Test_Has_Steps_Ahead_Wrapper (
      T : in out Traveller_Test'Class) is
   begin
      Test_Has_Steps_Ahead (T);
   end Test_Has_Steps_Ahead_Wrapper;

   procedure Test_Has_No_Step_Ahead_Wrapper (
      T : in out Traveller_Test'Class) is
   begin
      Test_Has_No_Step_Ahead (T);
   end Test_Has_No_Step_Ahead_Wrapper;

   procedure Test_Is_Travelling_Wrapper (
      T : in out Traveller_Test'Class) is
   begin
      Test_Is_Travelling (T);
   end Test_Is_Travelling_Wrapper;

   procedure Test_Is_Not_Travelling_Wrapper (
      T : in out Traveller_Test'Class) is
   begin
      Test_Is_Not_Travelling (T);
   end Test_Is_Not_Travelling_Wrapper;

   procedure Test_Equality_Of_Two_Travellers_Wrapper (
      T : in out Traveller_Test'Class) is
   begin
      Test_Equality_Of_Two_Travellers (T);
   end Test_Equality_Of_Two_Travellers_Wrapper;

   --procedure Test_Inequality_Of_Two_Travellers_Wrapper
   --  (T : in out Traveller_Test'Class) is
   --begin
   --   Test_Inequality_Of_Two_Travellers (T);
   --end Test_Inequality_Of_Two_Travellers_Wrapper;

    -----------------------------------------------------
   --                  REGISTRATION
   -----------------------------------------------------
   procedure Register_Tests (T: in out Traveller_Test) is
      package Register_Specific is
         new TC.Specific_Test_Case_Registration (Traveller_Test);
      use Register_Specific;
   begin
      Register_Wrapper (
         Test => T,
         Routine => Test_Traveller_Id_Getter_Wrapper'Access,
         Name => "Test Traveller id getter");

      Register_Wrapper (
         Test => T,
         Routine => Test_Position_Getter_Wrapper'Access,
         Name => "Test position getter");

      Register_Wrapper (
         Test => T,
         Routine => Test_Position_Setter_Wrapper'Access,
         Name => "Test position setter");

      Register_Wrapper (
         Test => T,
         Routine => Test_Maximum_Speed_Getter_Wrapper'Access,
         Name => "Test maximum speed getter");

      Register_Wrapper (
         Test => T,
         Routine => Test_Current_Speed_Getter_Wrapper'Access,
         Name => "Test current speed getter");

      Register_Wrapper (
         Test => T,
         Routine => Test_Current_Speed_Setter_Wrapper'Access,
         Name => "Test current speed setter");

      Register_Wrapper (
         Test => T,
         Routine => Test_Scheduled_For_Getter_Wrapper'Access,
         Name => "Test current speed getter");

      Register_Wrapper (
         Test => T,
         Routine => Test_Scheduled_For_Setter_Wrapper'Access,
         Name => "Test current speed setter");

      Register_Wrapper (
         Test => T,
         Routine => Test_Source_Getter_Wrapper'Access,
         Name => "Test source getter");

      Register_Wrapper (
         Test => T,
         Routine => Test_Destination_Getter_Wrapper'Access,
         Name => "Test destination getter");

      Register_Wrapper (
         Test => T,
         Routine => Test_Contains_Step_Infra_Id_Wrapper'Access,
         Name => "Test contains step operation with Infra_Id parameter");

      Register_Wrapper (
         Test => T,
         Routine => Test_Contains_Step_Slice_Wrapper'Access,
         Name => "Test contains step operation with Slice parameter");

      Register_Wrapper (
         Test => T,
         Routine => Test_Has_Steps_Ahead_Wrapper'Access,
         Name => "Test traveller has steps ahead");

      Register_Wrapper (
         Test => T,
         Routine => Test_Has_No_Step_Ahead_Wrapper'Access,
         Name => "Test traveller has no step ahead");

      Register_Wrapper (
         Test => T,
         Routine => Test_Is_Travelling_Wrapper'Access,
         Name => "Test is travelling");

      Register_Wrapper (
         Test => T,
         Routine => Test_Is_Not_Travelling_Wrapper'Access,
         Name => "Test is not travelling");

      Register_Wrapper (
         Test => T,
         Routine => Test_Equality_Of_Two_Travellers_Wrapper'Access,
         Name => "Test the equality of two travellers");

      --Register_Wrapper
      --  (Test => T,
      --   Routine => Test_Inequality_Of_Two_Travellers_Wrapper'Access,
      --   Name => "Test the inequality of two travellers");
   end Register_Tests;
end Active.Traveller.Tests;
