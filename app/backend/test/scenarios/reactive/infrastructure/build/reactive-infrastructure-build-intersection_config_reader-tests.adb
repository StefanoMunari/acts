with AUnit.Assertions;
with Ada.Strings.Unbounded;

-- gnatcoll libs
with GNATCOLL.JSON;

with Active.Agent;

with Reactive.Infrastructure.Intersection.Intersection_Builder.Mock;

with Shared.Direction;

package body Reactive.Infrastructure.Build.Intersection_Config_Reader.Tests is
   package Ass    renames AUnit.Assertions;
   package SU renames Ada.Strings.Unbounded;
   package G_JSON renames GNATCOLL.JSON;
   package Agent renames Active.Agent;
   package Intersection_Builder
      renames Reactive.Infrastructure.Intersection.Intersection_Builder;
   package Intersection_Builder_Mock renames Intersection_Builder.Mock;
   use Reactive.Infra_Id_Type;
   use Intersection_Builder;
   use Shared.Direction;

   -----------------------------------------------------
   --                   SCAFFOLDING
   -----------------------------------------------------

   ICR          : Intersection_Config_Reader.Reference;
   Builder_Mock : Intersection_Builder_Mock.Reference;

-- T-JUNCTION FIELDS
   Exit_T_1_Street       : Integer  := 11;
   Exit_T_1_Stretch      : Integer  := 101;
   Exit_T_1_TL           : Integer  := 41;
   Exit_T_1_Direction    : Cardinal := NORTH;
   Exit_T_2_Street       : Integer  := 12;
   Exit_T_2_Stretch      : Integer  := 102;
   Exit_T_2_TL           : Integer  := 42;
   Exit_T_2_Direction    : Cardinal := SOUTH;
   Exit_T_3_Street       : Integer  := 13;
   Exit_T_3_Stretch      : Integer  := 103;
   Exit_T_3_TL           : Integer  := 43;
   Exit_T_3_Direction    : Cardinal := EAST;

-- X-JUNCTION FIELDS
   Exit_X_1_Street       : Integer  := 21;
   Exit_X_1_Stretch      : Integer  := 201;
   Exit_X_1_TL           : Integer  := 51;
   Exit_X_1_Direction    : Cardinal := NORTH;
   Exit_X_2_Street       : Integer  := 22;
   Exit_X_2_Stretch      : Integer  := 202;
   Exit_X_2_TL           : Integer  := 52;
   Exit_X_2_Direction    : Cardinal := SOUTH;
   Exit_X_3_Street       : Integer  := 23;
   Exit_X_3_Stretch      : Integer  := 203;
   Exit_X_3_TL           : Integer  := 53;
   Exit_X_3_Direction    : Cardinal := EAST;
   Exit_X_4_Street       : Integer  := 24;
   Exit_X_4_Stretch      : Integer  := 204;
   Exit_X_4_TL           : Integer  := 54;
   Exit_X_4_Direction    : Cardinal := WEST;

   procedure Set_Up (T : in out Intersection_Config_Reader_Test) is
   begin
      Builder_Mock := Intersection_Builder_Mock.Create;
      ICR          := new Intersection_Config_Reader.Object;
   end Set_Up;

   function Get_JSON_For_T_Junction return G_JSON.JSON_Value
   is
      T_Junction : G_JSON.JSON_Value := G_JSON.Create_Object;
      Exits      : G_JSON.JSON_Array := G_JSON.Empty_Array;
      Exit_1     : G_JSON.JSON_Value := G_JSON.Create_Object;
      Exit_2     : G_JSON.JSON_Value := G_JSON.Create_Object;
      Exit_3     : G_JSON.JSON_Value := G_JSON.Create_Object;
      Dummy_N    : Integer := 1000000;
      Stretches  : G_JSON.JSON_Array;
      Single_Stretch : G_JSON.JSON_Array;
   begin
      Exit_1.Set_Field ("streetId", Exit_T_1_Street);
      Single_Stretch := G_JSON.Empty_Array;
      Stretches := G_JSON.Empty_Array;
      G_JSON.Append (Single_Stretch, G_JSON.Create (Dummy_N));
      G_JSON.Append (Single_Stretch, G_JSON.Create (Dummy_N));
      G_JSON.Append (Single_Stretch, G_JSON.Create (Exit_T_1_Stretch));
      G_JSON.Append (Stretches, G_JSON.Create (Single_Stretch));
      Exit_1.Set_Field ("stretchId", G_JSON.Create (Stretches));
      Exit_1.Set_Field ("trafficLightId", Exit_T_1_TL);
      Exit_1.Set_Field ("direction", Cardinal'Image (Exit_T_1_Direction));

      Exit_2.Set_Field ("streetId", Exit_T_2_Street);
      Single_Stretch := G_JSON.Empty_Array;
      Stretches := G_JSON.Empty_Array;
      G_JSON.Append (Single_Stretch, G_JSON.Create (Dummy_N));
      G_JSON.Append (Single_Stretch, G_JSON.Create (Dummy_N));
      G_JSON.Append (Single_Stretch, G_JSON.Create (Exit_T_2_Stretch));
      G_JSON.Append (Stretches, G_JSON.Create (Single_Stretch));
      Exit_2.Set_Field ("stretchId", G_JSON.Create (Stretches));
      Exit_2.Set_Field ("trafficLightId", Exit_T_2_TL);
      Exit_2.Set_Field ("direction", Cardinal'Image (Exit_T_2_Direction));

      Exit_3.Set_Field ("streetId", Exit_T_3_Street);
      Single_Stretch := G_JSON.Empty_Array;
      Stretches := G_JSON.Empty_Array;
      G_JSON.Append (Single_Stretch, G_JSON.Create (Dummy_N));
      G_JSON.Append (Single_Stretch, G_JSON.Create (Dummy_N));
      G_JSON.Append (Single_Stretch, G_JSON.Create (Exit_T_3_Stretch));
      G_JSON.Append (Stretches, G_JSON.Create (Single_Stretch));
      Exit_3.Set_Field ("stretchId", G_JSON.Create (Stretches));
      Exit_3.Set_Field ("trafficLightId", Exit_T_3_TL);
      Exit_3.Set_Field ("direction", Cardinal'Image (Exit_T_3_Direction));

      G_JSON.Append (Exits, Exit_1);
      G_JSON.Append (Exits, Exit_2);
      G_JSON.Append (Exits, Exit_3);
      T_Junction.Set_Field ("exits", Exits);

      return T_Junction;
   end Get_JSON_For_T_Junction;

   function Get_JSON_For_X_Junction return G_JSON.JSON_Value
   is
      X_Junction : G_JSON.JSON_Value := G_JSON.Create_Object;
      Exits      : G_JSON.JSON_Array := G_JSON.Empty_Array;
      Exit_1     : G_JSON.JSON_Value := G_JSON.Create_Object;
      Exit_2     : G_JSON.JSON_Value := G_JSON.Create_Object;
      Exit_3     : G_JSON.JSON_Value := G_JSON.Create_Object;
      Exit_4     : G_JSON.JSON_Value := G_JSON.Create_Object;
      Dummy_N    : Integer := 1000000;
      Stretches  : G_JSON.JSON_Array;
      Single_Stretch : G_JSON.JSON_Array;
   begin
      Exit_1.Set_Field ("streetId", Exit_X_1_Street);
      Single_Stretch := G_JSON.Empty_Array;
      Stretches := G_JSON.Empty_Array;
      G_JSON.Append (Single_Stretch, G_JSON.Create (Dummy_N));
      G_JSON.Append (Single_Stretch, G_JSON.Create (Dummy_N));
      G_JSON.Append (Single_Stretch, G_JSON.Create (Exit_X_1_Stretch));
      G_JSON.Append (Stretches, G_JSON.Create (Single_Stretch));
      Exit_1.Set_Field ("stretchId", G_JSON.Create (Stretches));
      Exit_1.Set_Field ("trafficLightId", Exit_X_1_TL);
      Exit_1.Set_Field ("direction", Cardinal'Image (Exit_X_1_Direction));

      Exit_2.Set_Field ("streetId", Exit_X_2_Street);
      Single_Stretch := G_JSON.Empty_Array;
      Stretches := G_JSON.Empty_Array;
      G_JSON.Append (Single_Stretch, G_JSON.Create (Dummy_N));
      G_JSON.Append (Single_Stretch, G_JSON.Create (Dummy_N));
      G_JSON.Append (Single_Stretch, G_JSON.Create (Exit_X_2_Stretch));
      G_JSON.Append (Stretches, G_JSON.Create (Single_Stretch));
      Exit_2.Set_Field ("stretchId", G_JSON.Create (Stretches));
      Exit_2.Set_Field ("trafficLightId", Exit_X_2_TL);
      Exit_2.Set_Field ("direction", Cardinal'Image (Exit_X_2_Direction));

      Exit_3.Set_Field ("streetId", Exit_X_3_Street);
      Single_Stretch := G_JSON.Empty_Array;
      Stretches := G_JSON.Empty_Array;
      G_JSON.Append (Single_Stretch, G_JSON.Create (Dummy_N));
      G_JSON.Append (Single_Stretch, G_JSON.Create (Dummy_N));
      G_JSON.Append (Single_Stretch, G_JSON.Create (Exit_X_3_Stretch));
      G_JSON.Append (Stretches, G_JSON.Create (Single_Stretch));
      Exit_3.Set_Field ("stretchId", G_JSON.Create (Stretches));
      Exit_3.Set_Field ("trafficLightId", Exit_X_3_TL);
      Exit_3.Set_Field ("direction", Cardinal'Image (Exit_X_3_Direction));

      Exit_4.Set_Field ("streetId", Exit_X_4_Street);
      Single_Stretch := G_JSON.Empty_Array;
      Stretches := G_JSON.Empty_Array;
      G_JSON.Append (Single_Stretch, G_JSON.Create (Dummy_N));
      G_JSON.Append (Single_Stretch, G_JSON.Create (Dummy_N));
      G_JSON.Append (Single_Stretch, G_JSON.Create (Exit_X_4_Stretch));
      G_JSON.Append (Stretches, G_JSON.Create (Single_Stretch));
      Exit_4.Set_Field ("stretchId", G_JSON.Create (Stretches));
      Exit_4.Set_Field ("trafficLightId", Exit_X_4_TL);
      Exit_4.Set_Field ("direction", Cardinal'Image (Exit_X_4_Direction));

      G_JSON.Append (Exits, Exit_1);
      G_JSON.Append (Exits, Exit_2);
      G_JSON.Append (Exits, Exit_3);
      G_JSON.Append (Exits, Exit_4);
      X_Junction.Set_Field ("exits", Exits);

      return X_Junction;
   end Get_JSON_For_X_Junction;

   -- Test Routines:
   procedure Test_Set_Builder (T: in out TC.Test_Case'Class) is
      Builder : Intersection_Builder.Reference
         := Intersection_Builder.Reference (Builder_Mock);
   begin
      ICR.Set_Builder (Builder);

      Ass.Assert (Builder = ICR.Builder,
                 "Builder was not set correctly");
   end Test_Set_Builder;

   procedure Test_Read_T (T: in out TC.Test_Case'Class) is
      Builder : Intersection_Builder.Reference
         := Intersection_Builder.Reference (Builder_Mock);
      T_Json          : G_JSON.JSON_Value;
      Intersection_Id : Infra_Id := 42;
      TL_1_Id         : Agent.Agent_Id
         := Agent.Create_Id_From_Natural (Exit_T_1_TL);
      TL_2_Id         : Agent.Agent_Id
         := Agent.Create_Id_From_Natural (Exit_T_2_TL);
      TL_3_Id         : Agent.Agent_Id
         := Agent.Create_Id_From_Natural (Exit_T_3_TL);
      Result          : Infra_Id;
   begin
      Builder_Mock.Set_Return_Value_For_Get_Result (Intersection_Id);
      ICR.Set_Builder (Builder);

      T_Json := Get_JSON_For_T_Junction;

      Result := ICR.Read (T_Json);

      Ass.Assert (Result = Intersection_Id,
                 "Something different from the intersection was read");
      Ass.Assert (
         Builder_Mock.Get_With_Street_Called (
            Infra_Id (Exit_T_1_Street), Exit_T_1_Direction),
        "Street #1 was not added to the intersection");
      Ass.Assert (
         Builder_Mock.Get_With_Street_Called (
            Infra_Id (Exit_T_2_Street), Exit_T_2_Direction),
        "Street #2 was not added to the intersection");
      Ass.Assert (
         Builder_Mock.Get_With_Street_Called (
            Infra_Id (Exit_T_3_Street), Exit_T_3_Direction),
        "Street #3 was not added to the intersection");
      Ass.Assert (
         Builder_Mock.Get_With_Traffic_Light_Called (
            TL_1_Id, Exit_T_1_Direction),
         "Traffic Light #1 was not added to the intersection");
      Ass.Assert (
         Builder_Mock.Get_With_Traffic_Light_Called (
            TL_2_Id, Exit_T_2_Direction),
         "Traffic Light #2 was not added to the intersection");
      Ass.Assert (
         Builder_Mock.Get_With_Traffic_Light_Called (
            TL_3_Id, Exit_T_3_Direction),
         "Traffic Light #3 was not added to the intersection");
   end Test_Read_T;

   procedure Test_Read_X (T: in out TC.Test_Case'Class) is
      Builder : Intersection_Builder.Reference
         := Intersection_Builder.Reference (Builder_Mock);
      X_Json          : G_JSON.JSON_Value;
      Intersection_Id : Infra_Id := 42;
      TL_1_Id         : Agent.Agent_Id
         := Agent.Create_Id_From_Natural (Exit_X_1_TL);
      TL_2_Id         : Agent.Agent_Id
         := Agent.Create_Id_From_Natural (Exit_X_2_TL);
      TL_3_Id         : Agent.Agent_Id
         := Agent.Create_Id_From_Natural (Exit_X_3_TL);
      TL_4_Id         : Agent.Agent_Id
         := Agent.Create_Id_From_Natural (Exit_X_4_TL);
      Result          : Infra_Id;
   begin
      Builder_Mock.Set_Return_Value_For_Get_Result (Intersection_Id);
      ICR.Set_Builder (Builder);

      X_Json := Get_JSON_For_X_Junction;

      Result := ICR.Read (X_Json);

      Ass.Assert (Result = Intersection_Id,
                 "Something different from the intersection was read");
      Ass.Assert (
         Builder_Mock.Get_With_Street_Called (
            Infra_Id (Exit_X_1_Street), Exit_X_1_Direction),
        "Street #1 was not added to the intersection");
      Ass.Assert (
         Builder_Mock.Get_With_Street_Called (
            Infra_Id (Exit_X_2_Street), Exit_X_2_Direction),
        "Street #2 was not added to the intersection");
      Ass.Assert (
         Builder_Mock.Get_With_Street_Called (
            Infra_Id (Exit_X_3_Street), Exit_X_3_Direction),
        "Street #3 was not added to the intersection");
      Ass.Assert (
         Builder_Mock.Get_With_Street_Called (
            Infra_Id (Exit_X_4_Street), Exit_X_4_Direction),
        "Street #4 was not added to the intersection");
      Ass.Assert (
         Builder_Mock.Get_With_Traffic_Light_Called (
            TL_1_Id, Exit_X_1_Direction),
         "Traffic Light #1 was not added to the intersection");
      Ass.Assert (
         Builder_Mock.Get_With_Traffic_Light_Called (
            TL_2_Id, Exit_X_2_Direction),
         "Traffic Light #2 was not added to the intersection");
      Ass.Assert (
         Builder_Mock.Get_With_Traffic_Light_Called (
            TL_3_Id, Exit_X_3_Direction),
         "Traffic Light #3 was not added to the intersection");
      Ass.Assert (
         Builder_Mock.Get_With_Traffic_Light_Called (
            TL_4_Id, Exit_X_4_Direction),
         "Traffic Light #4 was not added to the intersection");
   end Test_Read_X;

   procedure Register_Tests (T : in out Intersection_Config_Reader_Test) is
      use TC.Registration;
   begin

      Register_Routine (
         Test    => T,
         Routine => Test_Set_Builder'Access,
         Name    => "A builder can be set for the ICR");

      Register_Routine (
         Test    => T,
         Routine => Test_Read_T'Access,
         Name    => "The ICR can read a T intersection config");

      Register_Routine (
         Test    => T,
         Routine => Test_Read_X'Access,
         Name    => "The ICR can read an X intersection config");

   end Register_Tests;

   function Name (T : in Intersection_Config_Reader_Test)
      return AU.Message_String is
   begin
      return AU.Format ("Intersection_Config_Reader");
   end Name;

end Reactive.Infrastructure.Build.Intersection_Config_Reader.Tests;
