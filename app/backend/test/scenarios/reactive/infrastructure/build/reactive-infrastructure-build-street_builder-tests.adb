with AUnit.Assertions;

-- gnatcoll libs
with GNATCOLL.JSON;

with Reactive.District.Mock;
with Reactive.Infrastructure.Factory.Street_Factory.Mock;
with Reactive.Infrastructure.Lane.Mock;
with Reactive.Infrastructure.Stretch.Mock;
with Reactive.Infrastructure.Way.Bikeway.Utils.Mock;
with Reactive.Infrastructure.Way.Footway.Utils.Mock;
with Reactive.Infrastructure.Way.Roadway.Utils.Mock;

with Shared.Direction;

package body Reactive.Infrastructure.Build.Street_Builder.Tests is
   package Ass    renames AUnit.Assertions;
   package G_JSON renames GNATCOLL.JSON;
   package District_Mock renames Reactive.District.Mock;
   package Street_Factory_Mock
      renames Reactive.Infrastructure.Factory.Street_Factory.Mock;
   package Lane_Mock renames Reactive.Infrastructure.Lane.Mock;
   package Stretch_Mock renames Reactive.Infrastructure.Stretch.Mock;
   package Bikeway_Utils_Mock_Pkg
      renames Reactive.Infrastructure.Way.Bikeway.Utils.Mock;
   package Footway_Utils_Mock_Pkg
      renames Reactive.Infrastructure.Way.Footway.Utils.Mock;
   package Roadway_Utils_Mock_Pkg
      renames Reactive.Infrastructure.Way.Roadway.Utils.Mock;

   -----------------------------------------------------
   --                   SCAFFOLDING
   -----------------------------------------------------
   Build_Ref         : Street_Builder.Reference;
   Factory_Mock      : Street_Factory_Mock.Reference;
   District_Mock_Ref : District_Mock.Reference;

   Bikeway_Factory_Mock      : Street_Factory_Mock.Reference;
   Footway_Factory_Mock      : Street_Factory_Mock.Reference;
   Roadway_Factory_Mock      : Street_Factory_Mock.Reference;
   Bikeway_Utils             : Bikeway_Utils_Mock_Pkg.Reference;
   Footway_Utils             : Footway_Utils_Mock_Pkg.Reference;
   Roadway_Utils             : Roadway_Utils_Mock_Pkg.Reference;

   procedure Set_Up (T : in out Street_Builder_Test) is
   begin
      Factory_Mock      := Street_Factory_Mock.Create;

      District_Mock_Ref    := District_Mock.Create;
      Bikeway_Factory_Mock := Street_Factory_Mock.Create;
      Roadway_Factory_Mock := Street_Factory_Mock.Create;
      Footway_Factory_Mock := Street_Factory_Mock.Create;
      Bikeway_Utils := Bikeway_Utils_Mock_Pkg.Create;
      Footway_Utils := Footway_Utils_Mock_Pkg.Create;
      Roadway_Utils := Roadway_Utils_Mock_Pkg.Create;
      Build_Ref     := Street_Builder.Create (
         District        => Reactive.District.Reference (District_Mock_Ref),
         Bikeway_Factory => Street_Factory.Reference (Bikeway_Factory_Mock),
         Footway_Factory => Street_Factory.Reference (Footway_Factory_Mock),
         Roadway_Factory => Street_Factory.Reference (Roadway_Factory_Mock),
         Bikeway_Utils   => Bikeway_Utils,
         Footway_Utils   => Footway_Utils,
         Roadway_Utils   => Roadway_Utils);
   end Set_Up;

   -- Test Routines:
   procedure Test_Get_Stretches_Empty (T: in out TC.Test_Case'Class) is
      Stretch_Array : G_JSON.JSON_Array := G_JSON.Empty_Array;
      Stretch_List  : Infra_Id_List.List;
      Factory       : Street_Factory.Reference
         := Street_Factory.Reference (Factory_Mock);
      Lane_Id       : Infra_Id := 998;
   begin
      Stretch_List := Build_Ref.Get_Stretches (Stretch_Array, Factory, Lane_Id);
      Ass.Assert (Stretch_List.Is_Empty,
         "A stretch was created");
   end Test_Get_Stretches_Empty;

   procedure Test_Get_Stretches_One (T: in out TC.Test_Case'Class) is
      Stretch_Array    : G_JSON.JSON_Array := G_JSON.Empty_Array;
      Stretch_Value    : G_JSON.JSON_Value := G_JSON.Create_Object;
      Stretch_List     : Infra_Id_List.List;
      Stretch_Ref      : Reactive.Infrastructure.Stretch.Reference
         := Stretch.Reference (Stretch_Mock.Create);
      Stretch_Mock_Ref : Stretch_Mock.Reference
         := Stretch_Mock.Reference (Stretch_Ref);
      Stretch_Id       : constant Infra_Id := 40;
      Stretch_Size     : constant Natural  := 5;
      Traveller        : G_JSON.JSON_Value := G_JSON.Create_Object;
      Traveller_Id     : Integer           := 42;
      Travellers       : G_JSON.JSON_Array := G_JSON.Empty_Array;
      Factory          : Street_Factory.Reference
         := Street_Factory.Reference (Factory_Mock);
      Lane_Id       : Infra_Id := 998;
   begin
      G_JSON.Append (Travellers, G_JSON.Create (Traveller_Id));

      Stretch_Value.Set_Field ("id", Integer (Stretch_Id));
      Stretch_Value.Set_Field ("size", Integer (Stretch_Size));
      Stretch_Value.Set_Field ("travellers", Travellers);
      G_JSON.Append (Stretch_Array, Stretch_Value);

      Stretch_Mock_Ref.Set_Id (Stretch_Id);
      Factory_Mock.Set_Return_Value_Create_Stretch (Stretch_Ref);

      Stretch_List := Build_Ref.Get_Stretches (Stretch_Array, Factory, Lane_Id);

      Ass.Assert (not Stretch_List.Is_Empty,
         "A stretch was not created");
      Ass.Assert (Stretch_List.Contains (Stretch_Id),
         "Stretch was not in the returned values");
      Ass.Assert (District_Mock_Ref.Contains_Infrastructure (Stretch_Id),
         "Stretch was not inserted in the district");
   end Test_Get_Stretches_One;

   procedure Test_Get_Stretches_Multi (T: in out TC.Test_Case'Class) is
      Stretch_Array  : G_JSON.JSON_Array := G_JSON.Empty_Array;
      Stretch_List   : Infra_Id_List.List;
      Stretch1_Value : G_JSON.JSON_Value := G_JSON.Create_Object;
      Stretch2_Value : G_JSON.JSON_Value := G_JSON.Create_Object;
      Stretch3_Value : G_JSON.JSON_Value := G_JSON.Create_Object;
      Stretch1_Ref   : Reactive.Infrastructure.Stretch.Reference
         := Stretch.Reference (Stretch_Mock.Create);
      Stretch1_Mock_Ref : Stretch_Mock.Reference
         := Stretch_Mock.Reference (Stretch1_Ref);
      Stretch2_Ref   : Reactive.Infrastructure.Stretch.Reference
         := Stretch.Reference (Stretch_Mock.Create);
      Stretch2_Mock_Ref : Stretch_Mock.Reference
         := Stretch_Mock.Reference (Stretch2_Ref);
      Stretch3_Ref   : Reactive.Infrastructure.Stretch.Reference
         := Stretch.Reference (Stretch_Mock.Create);
      Stretch3_Mock_Ref : Stretch_Mock.Reference
         := Stretch_Mock.Reference (Stretch3_Ref);
      Stretch1_Id    : Infra_Id := 41;
      Stretch2_Id    : Infra_Id := 42;
      Stretch3_Id    : Infra_Id := 43;
      Stretch1_Size  : Natural := 1;
      Stretch2_Size  : Natural := 2;
      Stretch3_Size  : Natural := 3;
      Traveller_1_Id : Integer           := 223;
      Travellers_1   : G_JSON.JSON_Array := G_JSON.Empty_Array;
      Traveller_2_Id : Integer           := 224;
      Travellers_2   : G_JSON.JSON_Array := G_JSON.Empty_Array;
      Traveller_3_Id : Integer           := 225;
      Travellers_3   : G_JSON.JSON_Array := G_JSON.Empty_Array;
      Factory        : Street_Factory.Reference
         := Street_Factory.Reference (Factory_Mock);
      Lane_Id       : Infra_Id := 998;
   begin
      Stretch1_Value.Set_Field ("id", Integer (Stretch1_Id));
      Stretch1_Value.Set_Field ("size", Integer (Stretch1_Size));
      G_JSON.Append (Travellers_1, G_JSON.Create (Traveller_1_Id));
      Stretch1_Value.Set_Field ("travellers", Travellers_1);
      G_JSON.Append (Stretch_Array, Stretch1_Value);

      Stretch2_Value.Set_Field ("id", Integer (Stretch2_Id));
      Stretch2_Value.Set_Field ("size", Integer (Stretch2_Size));
      G_JSON.Append (Travellers_2, G_JSON.Create (Traveller_2_Id));
      Stretch2_Value.Set_Field ("travellers", Travellers_2);
      G_JSON.Append (Stretch_Array, Stretch2_Value);

      Stretch3_Value.Set_Field ("id", Integer (Stretch3_Id));
      Stretch3_Value.Set_Field ("size", Integer (Stretch3_Size));
      G_JSON.Append (Travellers_3, G_JSON.Create (Traveller_3_Id));
      Stretch3_Value.Set_Field ("travellers", Travellers_3);
      G_JSON.Append (Stretch_Array, Stretch3_Value);

      Stretch1_Mock_Ref.Set_Id (Stretch1_Id);
      Stretch2_Mock_Ref.Set_Id (Stretch2_Id);
      Stretch3_Mock_Ref.Set_Id (Stretch3_Id);

      Factory_Mock.Set_Return_Value_Create_Stretch (Stretch1_Ref);
      Factory_Mock.Set_Return_Value_Create_Stretch (Stretch2_Ref);
      Factory_Mock.Set_Return_Value_Create_Stretch (Stretch3_Ref);

      Stretch_List := Build_Ref.Get_Stretches (Stretch_Array, Factory, Lane_Id);

      Ass.Assert (not Stretch_List.Is_Empty,
         "A stretch was not created");
      Ass.Assert (Stretch_List.Contains (Stretch1_Id),
         "First stretch was not in the returned values");
      Ass.Assert (Stretch_List.Contains (Stretch2_Id),
         "Second stretch was not in the returned values");
      Ass.Assert (Stretch_List.Contains (Stretch3_Id),
         "Third stretch was not in the returned values");
      Ass.Assert (District_Mock_Ref.Contains_Infrastructure (Stretch1_Id),
         "First stretch was not inserted in the district");
      Ass.Assert (District_Mock_Ref.Contains_Infrastructure (Stretch2_Id),
         "Second stretch was not inserted in the district");
      Ass.Assert (District_Mock_Ref.Contains_Infrastructure (Stretch3_Id),
         "Third stretch was not inserted in the district");
   end Test_Get_Stretches_Multi;

   procedure Test_Get_Lanes_Empty (T: in out TC.Test_Case'Class) is
      Lanes_Array : G_JSON.JSON_Array := G_JSON.Empty_Array;
      Lanes_List  : Infra_Id_List.List;
      Factory       : Street_Factory.Reference
         := Street_Factory.Reference (Factory_Mock);
   begin
      Lanes_List := Build_Ref.Get_Lanes (Lanes_Array, Factory);
      Ass.Assert (Lanes_List.Is_Empty, "A lane was created");
   end Test_Get_Lanes_Empty;

   procedure Test_Get_Lanes_One (T: in out TC.Test_Case'Class) is
      Lane_Array : G_JSON.JSON_Array := G_JSON.Empty_Array;
      Lane_Value : G_JSON.JSON_Value := G_JSON.Create_Object;
      Lane_List  : Infra_Id_List.List;
      Lane_Ref   : Lane.Reference := Lane.Reference (Lane_Mock.Create);
      Lane_Mock_Ref : Lane_Mock.Reference
         := Lane_Mock.Reference (Lane_Ref);
      Lane_Id        : Infra_Id := 40;
      Lane_Direction : Shared.Direction.Straight
         := Shared.Direction.NORTH_SOUTH;
      Stretch_Array  : G_JSON.JSON_Array := G_JSON.Empty_Array;
      Factory       : Street_Factory.Reference
         := Street_Factory.Reference (Factory_Mock);
   begin
      Lane_Value.Set_Field ("id", Integer (Lane_Id));
      Lane_Value.Set_Field
         ("direction", Shared.Direction.Straight'Image (Lane_Direction));
      Lane_Value.Set_Field ("stretches", Stretch_Array);
      G_JSON.Append (Lane_Array, Lane_Value);
      Lane_Mock_Ref.Set_Id (Lane_Id);
      Factory_Mock.Set_Return_Value_Create_Lane (Lane_Ref);

      Lane_List := Build_Ref.Get_Lanes (Lane_Array, Factory);

      Ass.Assert (not Lane_List.Is_Empty,
         "A Lane was not created");
      Ass.Assert (Lane_List.Contains (Lane_Id),
         "Lane was not in the returned values");
      Ass.Assert (District_Mock_Ref.Contains_Infrastructure (Lane_Id),
         "Lane was not inserted in the district");
   end Test_Get_Lanes_One;

   procedure Test_Get_Lanes_Multi (T: in out TC.Test_Case'Class) is
      Lane_Array  : G_JSON.JSON_Array := G_JSON.Empty_Array;
      Lane_List   : Infra_Id_List.List;
      Lane1_Value : G_JSON.JSON_Value := G_JSON.Create_Object;
      Lane2_Value : G_JSON.JSON_Value := G_JSON.Create_Object;
      Lane3_Value : G_JSON.JSON_Value := G_JSON.Create_Object;
      Lane1_Ref   : Reactive.Infrastructure.Lane.Reference
         := Lane.Reference (Lane_Mock.Create);
      Lane1_Mock_Ref : Lane_Mock.Reference
         := Lane_Mock.Reference (Lane1_Ref);
      Lane2_Ref   : Reactive.Infrastructure.Lane.Reference
         := Lane.Reference (Lane_Mock.Create);
      Lane2_Mock_Ref : Lane_Mock.Reference
         := Lane_Mock.Reference (Lane2_Ref);
      Lane3_Ref   : Reactive.Infrastructure.Lane.Reference
         := Lane.Reference (Lane_Mock.Create);
      Lane3_Mock_Ref : Lane_Mock.Reference
         := Lane_Mock.Reference (Lane3_Ref);
      Lane1_Id    : Infra_Id := 41;
      Lane2_Id    : Infra_Id := 42;
      Lane3_Id    : Infra_Id := 43;
      Lane1_Direction : Shared.Direction.Straight
         := Shared.Direction.NORTH_SOUTH;
      Lane2_Direction : Shared.Direction.Straight
         := Shared.Direction.EAST_WEST;
      Lane3_Direction : Shared.Direction.Straight
         := Shared.Direction.WEST_EAST;
      Stretch_Array1  : G_JSON.JSON_Array := G_JSON.Empty_Array;
      Stretch_Array2  : G_JSON.JSON_Array := G_JSON.Empty_Array;
      Stretch_Array3  : G_JSON.JSON_Array := G_JSON.Empty_Array;
      Factory       : Street_Factory.Reference
         := Street_Factory.Reference (Factory_Mock);
   begin
      Lane1_Value.Set_Field ("id", Integer (Lane1_Id));
      Lane1_Value.Set_Field
         ("direction", Shared.Direction.Straight'Image (Lane1_Direction));
      Lane1_Value.Set_Field ("stretches", Stretch_Array1);
      G_JSON.Append (Lane_Array, Lane1_Value);

      Lane2_Value.Set_Field ("id", Integer (Lane2_Id));
      Lane2_Value.Set_Field
         ("direction", Shared.Direction.Straight'Image (Lane2_Direction));
      Lane2_Value.Set_Field ("stretches", Stretch_Array2);
      G_JSON.Append (Lane_Array, Lane2_Value);

      Lane3_Value.Set_Field ("id", Integer (Lane3_Id));
      Lane3_Value.Set_Field
         ("direction", Shared.Direction.Straight'Image (Lane3_Direction));
      Lane3_Value.Set_Field ("stretches", Stretch_Array3);
      G_JSON.Append (Lane_Array, Lane3_Value);

      Lane1_Mock_Ref.Set_Id (Lane1_Id);
      Lane2_Mock_Ref.Set_Id (Lane2_Id);
      Lane3_Mock_Ref.Set_Id (Lane3_Id);

      Factory_Mock.Set_Return_Value_Create_Lane (Lane1_Ref);
      Factory_Mock.Set_Return_Value_Create_Lane (Lane2_Ref);
      Factory_Mock.Set_Return_Value_Create_Lane (Lane3_Ref);

      Lane_List := Build_Ref.Get_Lanes (Lane_Array, Factory);

      Ass.Assert (not Lane_List.Is_Empty,
         "A lane was not created");
      Ass.Assert (Lane_List.Contains (Lane1_Id),
         "First lane was not in the returned values");
      Ass.Assert (Lane_List.Contains (Lane2_Id),
         "Second lane was not in the returned values");
      Ass.Assert (Lane_List.Contains (Lane3_Id),
         "Third lane was not in the returned values");
      Ass.Assert (District_Mock_Ref.Contains_Infrastructure (Lane1_Id),
         "First lane was not inserted in the district");
      Ass.Assert (District_Mock_Ref.Contains_Infrastructure (Lane2_Id),
         "Second lane was not inserted in the district");
      Ass.Assert (District_Mock_Ref.Contains_Infrastructure (Lane3_Id),
         "Third lane was not inserted in the district");
   end Test_Get_Lanes_Multi;

   procedure Test_With_Bikeway (T: in out TC.Test_Case'Class) is
      Bikeway_Value  : G_JSON.JSON_Value := G_JSON.Create_Object;
      Bikeway_Id     : Infra_Id := 42;
      Lanes_Array    : G_JSON.JSON_Array := G_JSON.Empty_Array;
      Bikeway_Id_Res : Infra_Id;
   begin
      Bikeway_Value.Set_Field ("id", Integer (Bikeway_Id));
      Bikeway_Value.Set_Field ("lanes", Lanes_Array);

      Bikeway_Id_Res := Build_Ref.With_Bikeway (Bikeway_Value);

      Ass.Assert (Bikeway_Id = Bikeway_Id_Res,
         "Something else was created");
      Ass.Assert (District_Mock_Ref.Contains_Infrastructure (Bikeway_Id),
         "Bikeway was not inserted in the district");
   end Test_With_Bikeway;

   procedure Test_With_Footway (T: in out TC.Test_Case'Class) is
      Footway_Value  : G_JSON.JSON_Value := G_JSON.Create_Object;
      Footway_Id     : Infra_Id := 42;
      Lanes_Array    : G_JSON.JSON_Array := G_JSON.Empty_Array;
      Footway_Id_Res : Infra_Id;
   begin
      Footway_Value.Set_Field ("id", Integer (Footway_Id));
      Footway_Value.Set_Field ("lanes", Lanes_Array);

      Footway_Id_Res := Build_Ref.With_Footway (Footway_Value);

      Ass.Assert (Footway_Id = Footway_Id_Res,
         "Something else was created");
      Ass.Assert (District_Mock_Ref.Contains_Infrastructure (Footway_Id),
         "Footway was not inserted in the district");
   end Test_With_Footway;

   procedure Test_With_Roadway (T: in out TC.Test_Case'Class) is
      Roadway_Value  : G_JSON.JSON_Value := G_JSON.Create_Object;
      Roadway_Id     : Infra_Id := 42;
      Lanes_Array    : G_JSON.JSON_Array := G_JSON.Empty_Array;
      Roadway_Id_Res : Infra_Id;
   begin
      Roadway_Value.Set_Field ("id", Integer (Roadway_Id));
      Roadway_Value.Set_Field ("lanes", Lanes_Array);

      Roadway_Id_Res := Build_Ref.With_Roadway (Roadway_Value);

      Ass.Assert (Roadway_Id = Roadway_Id_Res,
         "Something else was created");
      Ass.Assert (District_Mock_Ref.Contains_Infrastructure (Roadway_Id),
         "Roadway was not inserted in the district");
   end Test_With_Roadway;

   procedure Test_Get_Street (T: in out TC.Test_Case'Class) is
      Bikeway_Value  : G_JSON.JSON_Value := G_JSON.Create_Object;
      Bikeway_Id     : Infra_Id := 41;
      Footway_Value  : G_JSON.JSON_Value := G_JSON.Create_Object;
      Footway_Id     : Infra_Id := 42;
      Roadway_Value  : G_JSON.JSON_Value := G_JSON.Create_Object;
      Roadway_Id     : Infra_Id := 43;
      Lanes_Array    : G_JSON.JSON_Array := G_JSON.Empty_Array;
      Id_Res         : Infra_Id;
      Street_Value   : G_JSON.JSON_Value := G_JSON.Create_Object;
      Street_Id      : Infra_Id := 44;
      Orientation    : Shared.Direction.Orientation
         := Shared.Direction.HORIZONTAL;
      Street_Id_Res  : Infra_Id;
   begin
      Bikeway_Value.Set_Field ("id", Integer (Bikeway_Id));
      Bikeway_Value.Set_Field ("lanes", Lanes_Array);
      Id_Res := Build_Ref.With_Bikeway (Bikeway_Value);

      Footway_Value.Set_Field ("id", Integer (Footway_Id));
      Footway_Value.Set_Field ("lanes", Lanes_Array);
      Id_Res := Build_Ref.With_Footway (Footway_Value);

      Roadway_Value.Set_Field ("id", Integer (Roadway_Id));
      Roadway_Value.Set_Field ("lanes", Lanes_Array);
      Id_Res := Build_Ref.With_Roadway (Roadway_Value);

      Street_Value.Set_Field ("id", Integer (Street_Id));
      Street_Value.Set_Field ("orientation",
         Shared.Direction.Orientation'Image (Orientation));
      Street_Id_Res := Build_Ref.Get_Street (Street_Value);

      Ass.Assert (Street_Id = Street_Id_Res,
         "Something else was created");
      Ass.Assert (District_Mock_Ref.Contains_Infrastructure (Street_Id),
         "Street was not inserted in the district");
   end Test_Get_Street;

   procedure Register_Tests (T : in out Street_Builder_Test) is
      use TC.Registration;
   begin

      Register_Routine (Test    => T,
                        Routine => Test_Get_Stretches_Empty'Access,
                        Name    => "Test stretch getter with empty array");

      Register_Routine (Test    => T,
                        Routine => Test_Get_Stretches_One'Access,
                        Name    =>
                           "Test stretch getter with single-element array");

      Register_Routine (Test    => T,
                        Routine => Test_Get_Stretches_Multi'Access,
                        Name    =>
                           "Test stretch getter with multi-element array");

      Register_Routine (Test    => T,
                        Routine => Test_Get_Lanes_Empty'Access,
                        Name    => "Test lane getter with empty array");

      Register_Routine (Test    => T,
                        Routine => Test_Get_Lanes_One'Access,
                        Name    =>
                           "Test lane getter with single-element array");

      Register_Routine (Test    => T,
                        Routine => Test_Get_Lanes_Multi'Access,
                        Name    =>
                           "Test lane getter with multi-element array");

      Register_Routine (Test    => T,
                        Routine => Test_With_Bikeway'Access,
                        Name    =>
                           "Test building of a bikeway");

      Register_Routine (Test    => T,
                        Routine => Test_With_Footway'Access,
                        Name    =>
                           "Test building of a footway");

      Register_Routine (Test    => T,
                        Routine => Test_With_Roadway'Access,
                        Name    =>
                           "Test building of a roadway");

      Register_Routine (Test    => T,
                        Routine => Test_Get_Street'Access,
                        Name    =>
                           "Test creation of a street");

   end Register_Tests;

   function Name (T : in Street_Builder_Test)
      return AU.Message_String is
   begin
      return AU.Format ("Street_Builder");
   end Name;

end Reactive.Infrastructure.Build.Street_Builder.Tests;
