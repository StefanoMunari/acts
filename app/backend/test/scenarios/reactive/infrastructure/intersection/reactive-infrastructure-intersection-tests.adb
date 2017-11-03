with AUnit.Assertions;
with Ada.Text_IO;
with Ada.Strings.Unbounded;

with GNATCOLL.JSON;

with Reactive.Infrastructure.Intersection;
with Reactive.Infrastructure.Intersection.Crossing;
with Shared.Direction;

package body Reactive.Infrastructure.Intersection.Tests is
   package Ass renames AUnit.Assertions;
   package Direction renames Shared.Direction;
   package G_JSON renames GNATCOLL.JSON;
   package SU renames Ada.Strings.Unbounded;

   -----------------------------------------------------
   --                   SCAFFOLDING
   -----------------------------------------------------
   Intersection_Ref : Intersection.Reference;

   procedure Set_Up (T: in out Intersection_Test) is
   begin
      Intersection_Ref
        := new Intersection.Object;
   end Set_Up;

   -----------------------------------------------------
   --                   SCENARIOS
   -----------------------------------------------------
   procedure Test_Intersection_Id_Getter (T: in out TC.Test_Case'Class)
   is
      Intersection_Id : Infra_Id := 34;
   begin
      Intersection_Ref.Id := Intersection_Id;

      Ass.Assert (Intersection_Ref.Get_Id = Intersection_Id,
                  "The id returned by Intersection's getter is wrong");
   end Test_Intersection_Id_Getter;

   procedure Test_Intersection_Size_Getter (T: in out TC.Test_Case'Class)
   is
      Intersection_Size : Natural := 34;
   begin
      Intersection_Ref.Size := Intersection_Size;

      Ass.Assert (Intersection_Ref.Get_Size = Intersection_Size,
                  "The size returned by Intersection's getter is wrong");
   end Test_Intersection_Size_Getter;

   procedure Test_Intersection_Street_Id_Getter (T : in out TC.Test_Case'Class)
   is
      Street_Id : Infra_Id := 34;
      Direction : Shared.Direction.Cardinal := Shared.Direction.NORTH;
   begin
      Intersection_Ref.Streets (Direction) := Street_Id;

      Ass.Assert (Intersection_Ref.Get_Street (Direction) = Street_Id,
                  "The id returned by street id's getter is not equal to the set one");
   end Test_Intersection_Street_Id_Getter;

   procedure Test_Find_Intersections (T: in out TC.Test_Case'Class) is
      Intersection_Id : Infra_Id := 34;
   begin
      Intersection_Ref.Id := Intersection_Id;

      Ass.Assert (
         Intersection_Ref.Find_Intersections.Contains (Intersection_Id),
         "The id returned by Intersection's finder is not the one of the"
      & " intersection itself");

      Ass.Assert (Natural(Intersection_Ref.Find_Intersections.Length) = 1,
                  "There are intersections associated with this one");
   end Test_Find_Intersections;

   procedure Test_Streets_Count (T: in out TC.Test_Case'Class)
   is Streets_Count : Natural := 8475;
   begin
      Intersection_Ref.Streets_Count := Streets_Count;

      Ass.Assert (Intersection_Ref.Count_Streets = Streets_Count,
                  "The streets counter returns a number of streets different by the set one");
   end Test_Streets_Count;

   procedure Test_Equality_Of_Two_Intersections (T : in out TC.Test_Case'Class)
   is
      Intersection_Ref_Copy : Intersection.Object'Class := Intersection_Ref.all;
   begin
      Ass.Assert (Intersection_Ref."="(Intersection_Ref_Copy),
                  "The intersections are not equal");
   end Test_Equality_Of_Two_Intersections;

   procedure Test_Inequality_Of_Two_Intersections (T : in out TC.Test_Case'Class)
   is
      Intersection_Id : Infra_Id := 43534;
      Intersection2_Ref : Intersection.Reference;
      Intersection2_Id : Infra_Id := 44353;
   begin
      Intersection_Ref.Id := Intersection_Id;
      Intersection2_Ref := new Intersection.Object;
      Intersection2_Ref.Id := Intersection2_Id;

      Ass.Assert (not Intersection_Ref."="(Intersection2_Ref.all),
                  "The intersections are equal");
   end Test_Inequality_Of_Two_Intersections;

   procedure Test_Find_Street_Direction (T : in out TC.Test_Case'Class)
   is
      Street_Id : Infra_Id := 4534;
      Direction : Shared.Direction.Cardinal := Shared.Direction.WEST;
      Found_Direction : Shared.Direction.Cardinal;
      Direction_Found : Boolean;
   begin
      Intersection_Ref.Streets (Direction) := Street_Id;
      Intersection_Ref.Streets_Existence (Direction) := TRUE;

      Intersection_Ref.Find_Street_Direction (Street_Id => Street_Id,
                                              Street_Direction => Found_Direction,
                                              Found => Direction_Found);

      Ass.Assert (Direction_Found,
                  "No direction found");

      Ass.Assert (Shared.Direction."=" (Found_Direction, Direction),
                  "Direction found is not equal to set one");

   end Test_Find_Street_Direction;

   procedure Test_Not_Find_Street_Direction (T : in out TC.Test_Case'Class) is
      Found_Direction : Shared.Direction.Cardinal;
      Direction_Found : Boolean;
   begin
      Intersection_Ref.Find_Street_Direction (Street_Id => 4534,
                                              Street_Direction => Found_Direction,
                                              Found => Direction_Found);

      Ass.Assert (not Direction_Found,
                  "No direction found");
   end Test_Not_Find_Street_Direction;

   procedure Test_Find_Some_Street_Connected_With_Intersection
     (T : in out TC.Test_Case'Class)
   is
      Street_Id : Infra_Id := 4534;
      Direction : Shared.Direction.Cardinal := Shared.Direction.WEST;
      Streets_Connected_With_Intersection : Infra_Id_Set.Set;
   begin
      Intersection_Ref.Streets (Direction) := Street_Id;
      Intersection_Ref.Streets_Existence (Direction) := TRUE;

      Streets_Connected_With_Intersection :=
        Intersection_Ref.Find_Streets_Connected_With_Intersection;

      Ass.Assert (not Streets_Connected_With_Intersection.Is_Empty,
                  "The intersection is connected with no street");

      Ass.Assert (Natural(Streets_Connected_With_Intersection.Length) = 1,
                  "The intersection is connected with not only one street");

      Ass.Assert (Streets_Connected_With_Intersection.Contains (Street_Id),
                  "The intersection does not contains the street id provided");
   end Test_Find_Some_Street_Connected_With_Intersection;

   procedure Test_Find_No_Street_Connected_With_Intersection
     (T : in out TC.Test_Case'Class) is
   begin
      Ass.Assert (Intersection_Ref.Find_Streets_Connected_With_Intersection.Is_Empty,
                  "The intersection is connected with some street");
   end Test_Find_No_Street_Connected_With_Intersection;

   procedure Test_Street_Existence (T: in out TC.Test_Case'Class)
   is
      Direction : Shared.Direction.Cardinal := Shared.Direction.WEST;
   begin
      Intersection_Ref.Streets_Existence (Direction) := TRUE;

      Ass.Assert (Intersection_Ref.Exists_Street_For_Direction (Direction),
                  "No street is connected to intersection");
   end Test_Street_Existence;

   procedure Test_No_Street_Existence (T: in out TC.Test_Case'Class) is
   begin
      Ass.Assert (not Intersection_Ref.Exists_Street_For_Direction (Shared.Direction.WEST),
                  "Some street is connected to intersection");
   end Test_No_Street_Existence;

   procedure Test_Intersection_Id_Setter (T: in out TC.Test_Case'Class)
   is
      Intersection_Id : Infra_Id := 4534;
   begin
      Intersection_Ref.Set_Id (Intersection_Id);

      Ass.Assert ((Intersection_Ref.Id = Intersection_Id),
                  "The intersection id is not the one set");
   end Test_Intersection_Id_Setter;

   procedure Test_T_Junction_Type_Set (T: in out TC.Test_Case'Class) is
   begin
      Intersection_Ref.Set_Intersection_Type (Intersection.T_JUNCTION);

      Ass.Assert ((Intersection_Ref.Intersection_Type = Intersection.T_JUNCTION),
                  "The intersection is not a t junction");

      Ass.Assert ((Intersection_Ref.Size = 3),
                  "The intersection has not a size of 3");
   end Test_T_Junction_Type_Set;

   procedure Test_Crossroads_Type_Set (T: in out TC.Test_Case'Class) is
   begin
      Intersection_Ref.Set_Intersection_Type (Intersection.CROSSROADS);

      Ass.Assert ((Intersection_Ref.Intersection_Type = Intersection.CROSSROADS),
                  "The intersection is not a crossroads");

      Ass.Assert ((Intersection_Ref.Size = 4),
                  "The intersection has not a size of 4");
   end Test_Crossroads_Type_Set;

   procedure Test_Street_Connection (T : in out TC.Test_Case'Class)
   is
      Direction  : Shared.Direction.Cardinal := Shared.Direction.WEST;
      Street_Id  : Infra_Id := 36;
      Stretch_Id : Infra_Id := 37;
      Stretches  : Infra_Id_List.List := Infra_Id_List.Empty_List;
   begin
      Ass.Assert (not Intersection_Ref.Streets_Existence (Direction),
                  "A street for WEST direction aready exists");

      Stretches.Append (Street_Id);
      Intersection_Ref.Connect_Street (Street_Id => Street_Id,
                                       Stretches => Stretches,
                                       Direction => Direction);

      Ass.Assert (Intersection_Ref.Streets_Existence (Direction),
                  "A street for WEST direction not exists");

      Ass.Assert (Intersection_Ref.Streets (Direction) = Street_Id,
                  "The street for WEST direction has id equal to "
                  & Infra_Id'Image (Street_Id));

   end Test_Street_Connection;

   procedure Test_Increment_Streets (T : in out TC.Test_Case'Class)
   is
      Starting_Streets_Count : Natural := Intersection_Ref.Streets_Count;
   begin
      Intersection_Ref.Increment_Streets;

      Ass.Assert (Intersection_Ref.Streets_Count = Starting_Streets_Count+1,
                  "The streets quantity is not incremented");
   end Test_Increment_Streets;

   procedure Test_Is_Fully_Connected (T : in out TC.Test_Case'Class) is
   begin
      Intersection_Ref.Size := 3;
      Intersection_Ref.Streets_Count := Intersection_Ref.Size;

      Ass.Assert (Intersection_Ref.Is_Fully_Connected,
                  "The intersection is not fully connected");

      Ass.Assert (not Intersection_Ref.Is_Not_Fully_Connected,
                  "The intersection is not fully connected");
   end Test_Is_Fully_Connected;

   procedure Test_Is_Not_Fully_Connected (T : in out TC.Test_Case'Class) is
   begin
      Intersection_Ref.Size := 4;
      Intersection_Ref.Streets_Count := 3;

      Ass.Assert (Intersection_Ref.Is_Not_Fully_Connected,
                  "The intersection is fully connected");

      Ass.Assert (not Intersection_Ref.Is_Fully_Connected,
                  "The intersection is fully connected");
   end Test_Is_Not_Fully_Connected;

   procedure Test_Dump (T : in out TC.Test_Case'Class) is
      Data : G_JSON.JSON_Value := G_JSON.Create_Object;
      Intersection_Id : Infra_Id := 34;
      Exits_JSON : G_JSON.JSON_Array := G_JSON.Empty_Array;
      Exit_JSON  : G_JSON.JSON_Value;
      Street_Id  : Infra_Id := 20;
       -- traffic light id
      TL_Id      : Agent.Agent_Id := SU.To_Unbounded_String ("A");
      TL_Array   : Reactive.Infrastructure.Intersection.Crossing.Traffic_Lights_Array :=
                  (TL_Id, TL_Id, TL_Id, TL_Id);
       -- traveler id
      TR_Id      : Agent.Agent_Id := SU.To_Unbounded_String ("X");
      Entered    : Boolean := False;
      Already_In : Boolean := False;
      Dump_Result : G_JSON.JSON_Value;
      Test_Result : Boolean := True;
   begin
      -- ID
      Data.Set_Field (Field_Name => Id_Field,
                      Field      => Natural (Intersection_Id));
      -- Exits (4)
      -- 1st
      Exit_JSON := G_JSON.Create_Object;
      Exit_JSON.Set_Field (Field_Name => Street_Id_Field,
                           Field      => Natural (Street_Id));
      Exit_JSON.Set_Field (Field_Name => Direction_Field,
                           Field      =>
                              Direction.Cardinal'Image (Direction.NORTH));
      Exit_JSON.Set_Field (Field_Name => Traffic_Light_Field,
                           Field      => TL_Id);
      Exit_JSON.Set_Field (Field_Name => Traveller_Field,
                           Field      => TR_Id);
      -- add to intersection
      G_JSON.Append (Exits_JSON, Exit_JSON);
      -- update
      Exit_JSON := G_JSON.Create_Object;
      Street_Id := 21;
      TR_Id := SU.To_Unbounded_String ("Y");
      -- 2nd
      Exit_JSON.Set_Field (Field_Name => Street_Id_Field,
                           Field      => Natural (Street_Id));
      Exit_JSON.Set_Field (Field_Name => Direction_Field,
                           Field      =>
                              Direction.Cardinal'Image (Direction.SOUTH));
      Exit_JSON.Set_Field (Field_Name => Traffic_Light_Field,
                           Field      => TL_Id);
      Exit_JSON.Set_Field (Field_Name => Traveller_Field,
                           Field      => TR_Id);
      -- add to intersection
      G_JSON.Append (Exits_JSON, Exit_JSON);
      -- update
      Exit_JSON := G_JSON.Create_Object;
      Street_Id := 22;
      TR_Id := SU.To_Unbounded_String ("Z");
      -- 3rd
      Exit_JSON.Set_Field (Field_Name => Street_Id_Field,
                           Field      => Natural (Street_Id));
      Exit_JSON.Set_Field (Field_Name => Direction_Field,
                           Field      =>
                              Direction.Cardinal'Image (Direction.WEST));
      Exit_JSON.Set_Field (Field_Name => Traffic_Light_Field,
                           Field      => TL_Id);
      Exit_JSON.Set_Field (Field_Name => Traveller_Field,
                           Field      => TR_Id);
      -- add to intersection
      G_JSON.Append (Exits_JSON, Exit_JSON);
      -- update
      Exit_JSON := G_JSON.Create_Object;
      Street_Id := 23;
      -- 4th
      Exit_JSON.Set_Field (Field_Name => Street_Id_Field,
                           Field      => Natural (Street_Id));
      Exit_JSON.Set_Field (Field_Name => Direction_Field,
                           Field      =>
                              Direction.Cardinal'Image (Direction.EAST));
      Exit_JSON.Set_Field (Field_Name => Traffic_Light_Field,
                           Field      => TL_Id);
      -- add to exit
      G_JSON.Append (Exits_JSON, Exit_JSON);
      -- add to intersection data
      Data.Set_Field (Field_Name => Exits_Field,
                      Field      => Exits_JSON);
      ----
      -- Set Intersection Object
      ----
      Intersection_Ref.Crossing_Strategy :=
         new Infrastructure.Intersection.Crossing.Object;
      Intersection_Ref.Crossing_Strategy.Set_Traffic_Light
         (TL_Id, Direction.NORTH);
      Intersection_Ref.Crossing_Strategy.Set_Traffic_Light
         (TL_Id, Direction.SOUTH);
      Intersection_Ref.Crossing_Strategy.Set_Traffic_Light
         (TL_Id, Direction.WEST);
      Intersection_Ref.Crossing_Strategy.Set_Traffic_Light
         (TL_Id, Direction.EAST);
      Intersection_Ref.Id := Intersection_Id;
      -- (N, S, W ,E)
      Intersection_Ref.Streets := (20, 21, 22 ,23);
      -- (N, S, W ,E)
      Intersection_Ref.Streets_Existence := (True, True, True, True);
      -- Set Travellers
      Intersection_Ref.Entries.Try_To_Enter(
         SU.To_Unbounded_String ("X"), Direction.NORTH, Entered, Already_In);
      Ass.Assert (Entered and not Already_In,
         "Try_To_Enter fails to insert the NORTH Traveller");
      Intersection_Ref.Entries.Try_To_Enter(
         SU.To_Unbounded_String ("Y"), Direction.SOUTH, Entered, Already_In);
      Ass.Assert (Entered and not Already_In,
         "Try_To_Enter fails to insert the SOUTH Traveller");
      Intersection_Ref.Entries.Try_To_Enter(
         SU.To_Unbounded_String ("Z"), Direction.WEST, Entered, Already_In);
      Ass.Assert (Entered and not Already_In,
         "Try_To_Enter fails to insert the WEST Traveller");
      Intersection_Ref.Crossing_Strategy.Initialize (TL_Array);
      -- Test Dump
      Dump_Result := Intersection_Ref.Dump;
      -- Check Result
      Test_Result := Test_Result and (Dump_Result.Write = Data.Write);

      Ass.Assert (Test_Result,
            "The generated dump differs from the expected one (mismatch)");
   end Test_Dump;

   -----------------------------------------------------
   --                  REGISTRATION
   -----------------------------------------------------
   procedure Register_Tests (T: in out Intersection_Test) is
      use TC.Registration;
   begin
      Register_Routine (Test => T,
                        Routine => Test_Intersection_Id_Getter'Access,
                        Name => "Test intersection id getter");

      Register_Routine (Test => T,
                        Routine => Test_Intersection_Size_Getter'Access,
                        Name => "Test intersection size getter");

      Register_Routine (Test => T,
                        Routine => Test_Intersection_Street_Id_Getter'Access,
                        Name => "Test intersection's street id");

      Register_Routine
        (Test => T,
         Routine => Test_Find_Intersections'Access,
         Name => "Test if Intersections' finder returns its id");

      Register_Routine (Test => T,
                        Routine => Test_Streets_Count'Access,
                        Name => "Test streets count");

      Register_Routine
        (Test => T,
         Routine => Test_Equality_Of_Two_Intersections'Access,
         Name => "Test the equality of two intersections");

      Register_Routine
        (Test => T,
         Routine => Test_Inequality_Of_Two_Intersections'Access,
         Name => "Test the inquality of two intersections");

      Register_Routine
        (Test => T,
         Routine => Test_Find_Street_Direction'Access,
         Name => "Test find street direction");

      Register_Routine
        (Test => T,
         Routine => Test_Not_Find_Street_Direction'Access,
         Name => "Test not find street direction");

      Register_Routine
        (Test => T,
         Routine => Test_Find_Some_Street_Connected_With_Intersection'Access,
         Name => "Test find some street connected with intersection");

      Register_Routine
        (Test => T,
         Routine => Test_Find_No_Street_Connected_With_Intersection'Access,
         Name => "Test find no street connected with intersection");

      Register_Routine
        (Test => T,
         Routine => Test_Street_Existence'Access,
         Name => "Test street existence");

      Register_Routine
        (Test => T,
         Routine => Test_No_Street_Existence'Access,
         Name => "Test no street existence");

      Register_Routine
        (Test => T,
         Routine => Test_Intersection_Id_Setter'Access,
         Name => "Test the intersection id set");

      Register_Routine
        (Test => T,
         Routine => Test_T_Junction_Type_Set'Access,
         Name => "Test the intersection is a t junction");

      Register_Routine
        (Test => T,
         Routine => Test_Crossroads_Type_Set'Access,
         Name => "Test the intersection is a crossroads");

      Register_Routine
        (Test => T,
         Routine => Test_Street_Connection'Access,
         Name => "Test street connector");

      Register_Routine
        (Test => T,
         Routine => Test_Increment_Streets'Access,
         Name => "Test increment streets");

      Register_Routine
        (Test => T,
         Routine => Test_Is_Fully_Connected'Access,
         Name => "Test is fully onnected");

      Register_Routine
        (Test => T,
         Routine => Test_Is_Not_Fully_Connected'Access,
         Name => "Test is not fully connected");

      Register_Routine
        (Test => T,
         Routine => Test_Dump'Access,
         Name => "Test dump");

   end Register_Tests;

   function Name(T: Intersection_Test) return AU.Message_String is
   begin
      return AU.Format ("Intersection");
   end Name;
end Reactive.Infrastructure.Intersection.Tests;
