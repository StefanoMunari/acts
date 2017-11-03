with Ada.Tags; use Ada.Tags;

with AUnit.Assertions;

with Reactive.District.Mock;
with Reactive.Infrastructure.Factory.Street_Factory.Bikeway_Factory;
with Reactive.Infrastructure.Lane.Decoration.Lane_Sign_Decorator;
with Reactive.Infrastructure.Lane.Mock;
with Reactive.Infrastructure.Stretch.Decoration.Bicycle_Crossing;
with Reactive.Infrastructure.Stretch.Decoration.Pedestrian_Crossing;
with Reactive.Infrastructure.Stretch.Decoration.Stretch_Sign_Decorator;
with Reactive.Infrastructure.Building.Host.Facility;
with Reactive.Infrastructure.Building.Host.Utils.Mock;
with Reactive.Infrastructure.Building.Parking_Manager;
with Reactive.Infrastructure.Building.Parking_Manager.Garage;
with Reactive.Infrastructure.Stretch.Mock;

package body Reactive.Infrastructure.Factory.Street_Factory.Tests is
   package Ass renames AUnit.Assertions;

   package Bicycle_Crossing
      renames Reactive.Infrastructure.Stretch.Decoration.Bicycle_Crossing;
   package Pedestrian_Crossing
      renames Reactive.Infrastructure.Stretch.Decoration.Pedestrian_Crossing;
   package Stretch_Sign_Decorator
   renames Reactive.Infrastructure.Stretch.Decoration.Stretch_Sign_Decorator;
   package Facility
      renames Reactive.Infrastructure.Building.Host.Facility;
   package Lane_Sign_Decorator
      renames Reactive.Infrastructure.Lane.Decoration.Lane_Sign_Decorator;

   package District_Mock renames Reactive.District.Mock;
   package Host_Utils_Mock
      renames Reactive.Infrastructure.Building.Host.Utils.Mock;
   package Lane_Mock renames Reactive.Infrastructure.Lane.Mock;
   use Lane;
   package Stretch_Mock renames Reactive.Infrastructure.Stretch.Mock;
   use Stretch;

   -----------------------------------------------------
   --                   SCAFFOLDING
   -----------------------------------------------------
   Factory_Ref      : Reactive.Infrastructure.Factory.Street_Factory.Reference;
   District_Ref     : District_Mock.Reference;
   Host_Utils_Ref   : Host_Utils_Mock.Reference;
   Stretch_Mock_Ref : Stretch_Mock.Reference;
   Lane_Mock_Ref    : Lane_Mock.Reference;

   procedure Set_Up (T : in out Street_Factory_Test) is
   begin
      District_Ref     := District_Mock.Create;
      Host_Utils_Ref   := Host_Utils_Mock.Reference (Host_Utils_Mock.Create);
      Stretch_Mock_Ref := Stretch_Mock.Create;
      Lane_Mock_Ref    := Lane_Mock.Create;
      Factory_Ref      := new Bikeway_Factory.Object;

      Factory_Ref.Init (District_Ref);
   end Set_Up;

   -- Test Routines:
   procedure Test_Decorate_Stretch_No_Decoration (T: in out TC.Test_Case'Class)
   is
      Decoration_Value  : G_JSON.JSON_Value := G_JSON.Create_Object;
      Input_Stretch     : Stretch.Reference
         := Stretch.Reference (Stretch_Mock_Ref);
      Decorated_Stretch : Stretch.Reference;
   begin
      Decorated_Stretch :=
      -- static dispatching for this call
         Street_Factory.Decorate_Stretch (Factory_Ref.all,
                                          Input_Stretch,
                                          Decoration_Value);
      Ass.Assert (Decorated_Stretch = Input_Stretch,
                 "Stretch has changed");
   end Test_Decorate_Stretch_No_Decoration;

   procedure Test_Decorate_Stretch_Ped_Crossing (T: in out TC.Test_Case'Class)
   is
      Decoration_Value  : G_JSON.JSON_Value := G_JSON.Create_Object;
      Input_Stretch     : Stretch.Reference
         := Stretch.Reference (Stretch_Mock_Ref);
      Decorated_Stretch : Stretch.Reference;
      Decorator         : Stretch_Decorator.Reference;
   begin
      Decoration_Value.Set_Field ("pedestrianCrossing", "true");
      Decorated_Stretch :=
      -- static dispatching for this call
         Street_Factory.Decorate_Stretch (Factory_Ref.all,
                                          Input_Stretch,
                                          Decoration_Value);
      Decorator := Stretch_Decorator.Reference (Decorated_Stretch);
      Ass.Assert (Decorated_Stretch /= Input_Stretch,
                 "Stretch is the same as before");
      Ass.Assert (Decorator.Get_Stretch_Ref = Input_Stretch,
                 "Decorator correctly refers to input stretch");
      Ass.Assert (Decorator.all'Tag = Pedestrian_Crossing.Object'Tag,
                 "Decorator is not a pedestrian crossing");
   end Test_Decorate_Stretch_Ped_Crossing;

   procedure Test_Decorate_Stretch_Bike_Crossing (T: in out TC.Test_Case'Class)
   is
      Decoration_Value  : G_JSON.JSON_Value := G_JSON.Create_Object;
      Input_Stretch     : Stretch.Reference
         := Stretch.Reference (Stretch_Mock_Ref);
      Decorated_Stretch : Stretch.Reference;
      Decorator         : Stretch_Decorator.Reference;
   begin
      Decoration_Value.Set_Field ("bicycleCrossing", "true");
      Decorated_Stretch :=
      -- static dispatching for this call
         Street_Factory.Decorate_Stretch (Factory_Ref.all,
                                          Input_Stretch,
                                          Decoration_Value);
      Decorator := Stretch_Decorator.Reference (Decorated_Stretch);
      Ass.Assert (Decorated_Stretch /= Input_Stretch,
                 "Stretch is the same as before");
      Ass.Assert (Decorator.Get_Stretch_Ref = Input_Stretch,
                 "Decorator correctly refers to input stretch");
      Ass.Assert (Decorator.all'Tag = Bicycle_Crossing.Object'Tag,
                 "Decorator is not a bicycle crossing");
   end Test_Decorate_Stretch_Bike_Crossing;

   procedure Test_Decorate_Stretch_Bus_Stop (T: in out TC.Test_Case'Class)
   is
      Decoration_Value  : G_JSON.JSON_Value := G_JSON.Create_Object;
      Input_Stretch     : Stretch.Reference
         := Stretch.Reference (Stretch_Mock_Ref);
      Decorated_Stretch : Stretch.Reference;
      Decorator         : Stretch_Decorator.Reference;
      Bus_Stops         : G_JSON.JSON_Array := G_JSON.Empty_Array;
      Bus1_Id           : Integer := 42;
      Bus1_Waiting      : G_JSON.JSON_Array := G_JSON.Empty_Array;
      Bus1_Stops        : G_JSON.JSON_Array := G_JSON.Empty_Array;
      Bus1_Value        : G_JSON.JSON_Value := G_JSON.Create_Object;
      Bus2_Id           : Integer := 43;
      Bus2_Waiting      : G_JSON.JSON_Array := G_JSON.Empty_Array;
      Bus2_Stops        : G_JSON.JSON_Array := G_JSON.Empty_Array;
      Bus2_Value        : G_JSON.JSON_Value := G_JSON.Create_Object;
      Bus_Stop_Int      : Integer;
   begin
      Bus1_Value.Set_Field ("id", Bus1_Id);
      Bus1_Value.Set_Field ("waitingList", Bus1_Waiting);
      Bus_Stop_Int := 400;
      G_JSON.Append (Bus1_Stops, G_JSON.Create (Bus_Stop_Int));
      Bus_Stop_Int := 401;
      G_JSON.Append (Bus1_Stops, G_JSON.Create (Bus_Stop_Int));
      Bus_Stop_Int := 402;
      G_JSON.Append (Bus1_Stops, G_JSON.Create (Bus_Stop_Int));
      Bus1_Value.Set_Field ("stops", Bus1_Stops);

      Bus2_Value.Set_Field ("id", Bus2_Id);
      G_JSON.Append (Bus2_Waiting, G_JSON.Create ("8"));
      Bus2_Value.Set_Field ("waitingList", Bus2_Waiting);
      Bus_Stop_Int := 410;
      G_JSON.Append (Bus2_Stops, G_JSON.Create (Bus_Stop_Int));
      Bus_Stop_Int := 411;
      G_JSON.Append (Bus2_Stops, G_JSON.Create (Bus_Stop_Int));
      Bus_Stop_Int := 412;
      G_JSON.Append (Bus2_Stops, G_JSON.Create (Bus_Stop_Int));
      Bus2_Value.Set_Field ("stops", Bus2_Stops);

      G_JSON.Append (Bus_Stops, Bus1_Value);
      G_JSON.Append (Bus_Stops, Bus2_Value);
      Decoration_Value.Set_Field ("busStop", Bus_Stops);

      Decorated_Stretch :=
      -- static dispatching for this call
         Street_Factory.Decorate_Stretch (Factory_Ref.all,
                                          Input_Stretch,
                                          Decoration_Value);
      Decorator := Stretch_Decorator.Reference (Decorated_Stretch);
      Ass.Assert (Decorated_Stretch /= Input_Stretch,
                 "Stretch is the same as before");
      Ass.Assert (Decorator.Get_Stretch_Ref = Input_Stretch,
                 "Decorator correctly refers to input stretch");
      Ass.Assert (Decorator.all'Tag = Stretch_Sign_Decorator.Object'Tag,
                 "Decorator is not a bus stop");
   end Test_Decorate_Stretch_Bus_Stop;

   procedure Test_Decorate_Stretch_Facility (T: in out TC.Test_Case'Class)
   is
      Decoration_Value  : G_JSON.JSON_Value := G_JSON.Create_Object;
      Input_Stretch     : Stretch.Reference
         := Stretch.Reference (Stretch_Mock_Ref);
      Decorated_Stretch : Stretch.Reference;
      Facility_Id       : Infra_Id := 55;
   begin
      Decoration_Value.Set_Field ("facilityId", Integer (Facility_Id));

      Decorated_Stretch :=
      -- static dispatching for this call
         Street_Factory.Decorate_Stretch (Factory_Ref.all,
                                          Input_Stretch,
                                          Decoration_Value);

      Ass.Assert (Decorated_Stretch = Input_Stretch,
                 "Stretch isn't the same as before");
      Ass.Assert (Stretch_Mock_Ref.Get_Value_For_Set_Host_Id = Facility_Id,
                 "Stretch isn't the same as before");
   end Test_Decorate_Stretch_Facility;

   procedure Test_Decorate_Lane_No_Decoration (T: in out TC.Test_Case'Class)
   is
      Decoration_Value  : G_JSON.JSON_Value := G_JSON.Create_Object;
      Input_Lane        : Lane.Reference
         := Lane.Reference (Lane_Mock_Ref);
      Decorated_Lane    : Lane.Reference;
   begin
      Decorated_Lane :=
      -- static dispatching for this call
         Street_Factory.Decorate_Lane (Factory_Ref.all,
                                       Input_Lane,
                                       Decoration_Value);
      Ass.Assert (Decorated_Lane = Input_Lane,
                 "Lane has changed");
   end Test_Decorate_Lane_No_Decoration;

   procedure Test_Decorate_Lane_Speed_Limit (T: in out TC.Test_Case'Class)
   is
      Decoration_Value  : G_JSON.JSON_Value := G_JSON.Create_Object;
      Input_Lane        : Lane.Reference
         := Lane.Reference (Lane_Mock_Ref);
      Decorated_Lane    : Lane.Reference;
      Decorator         : Lane_Decorator.Reference;
   begin
      Decoration_Value.Set_Field ("speedLimit", Integer (100));
      Decorated_Lane :=
      -- static dispatching for this call
         Street_Factory.Decorate_Lane (Factory_Ref.all,
                                       Input_Lane,
                                       Decoration_Value);
      Decorator := Lane_Decorator.Reference (Decorated_Lane);
      Ass.Assert (Decorated_Lane /= Input_Lane,
                 "Lane is the same as before");
      Ass.Assert (Decorator.Get_Lane_Ref = Input_Lane,
                 "Decorator correctly refers to input lane");
      Ass.Assert (Decorator.all'Tag = Lane_Sign_Decorator.Object'Tag,
                 "Decorator is not a speed limit sign");
   end Test_Decorate_Lane_Speed_Limit;

   procedure Register_Tests (T : in out Street_Factory_Test) is
      use TC.Registration;
   begin

      Register_Routine (Test    => T,
                        Routine => Test_Decorate_Stretch_No_Decoration'Access,
                        Name    =>
                           "Test stretch ''empty'' decoration");

      Register_Routine (Test    => T,
                        Routine => Test_Decorate_Stretch_Ped_Crossing'Access,
                        Name    =>
                           "Test stretch pedestrian crossing decoration");

      Register_Routine (Test    => T,
                        Routine => Test_Decorate_Stretch_Bike_Crossing'Access,
                        Name    =>
                           "Test stretch bicycle crossing decoration");

      Register_Routine (Test    => T,
                        Routine => Test_Decorate_Stretch_Bus_Stop'Access,
                        Name    =>
                           "Test stretch bus stop decoration");

      Register_Routine (Test    => T,
                        Routine => Test_Decorate_Stretch_Facility'Access,
                        Name    =>
                           "Test stretch facility decoration");

      Register_Routine (Test    => T,
                        Routine => Test_Decorate_Lane_No_Decoration'Access,
                        Name    =>
                           "Test lane ''empty'' decoration");

      Register_Routine (Test    => T,
                        Routine => Test_Decorate_Lane_Speed_Limit'Access,
                        Name    =>
                           "Test lane speed limit decoration");

   end Register_Tests;

   function Name (T : in Street_Factory_Test)
      return AU.Message_String is
   begin
      return AU.Format ("Street_Factory");
   end Name;

end Reactive.Infrastructure.Factory.Street_Factory.Tests;
