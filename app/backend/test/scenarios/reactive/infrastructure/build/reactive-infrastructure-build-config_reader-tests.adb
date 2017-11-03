with Ada.Environment_Variables;
with AUnit.Assertions;

-- gnatcoll libs
with GNATCOLL.JSON;

with Reactive.Infrastructure.Build.Host_Config_Reader.Mock;
with Reactive.Infrastructure.Build.Intersection_Config_Reader.Mock;
with Reactive.Infrastructure.Build.Street_Config_Reader.Mock;

with Shared.Direction;
with Shared.Infra_Id_List;
with Shared.Reader.Mock;

package body Reactive.Infrastructure.Build.Config_Reader.Tests is
   package EV                 renames Ada.Environment_Variables;
   package Ass                renames AUnit.Assertions;
   package G_JSON             renames GNATCOLL.JSON;
   package Shared_Reader_Mock renames Shared.Reader.Mock;
   package Host_Config_Reader_Mock
      renames Reactive.Infrastructure.Build.Host_Config_Reader.Mock;
   package Street_Config_Reader_Mock
      renames Reactive.Infrastructure.Build.Street_Config_Reader.Mock;
   package Intersection_Config_Reader_Mock
      renames Reactive.Infrastructure.Build.Intersection_Config_Reader.Mock;
   package Infra_Id_List      renames Shared.Infra_Id_List;
   use Reactive.Infra_Id_Type;
   use Shared.Direction;

   C_Reader : Config_Reader.Reference;        -- config reader <= test this
   F_Reader : Shared.Reader.Reference;        -- file reader
   H_Reader : Host_Config_Reader.Reference; -- street reader
   S_Reader : Street_Config_Reader.Reference; -- street reader
   I_Reader : Intersection_Config_Reader.Reference; -- intersection reader

-- T-JUNCTION FIELDS
   Exit_T_1_Street       : Integer  := 11;
   Exit_T_1_Direction    : Cardinal := NORTH;
   Exit_T_2_Street       : Integer  := 12;
   Exit_T_2_Direction    : Cardinal := SOUTH;
   Exit_T_3_Street       : Integer  := 13;
   Exit_T_3_Direction    : Cardinal := EAST;

-- X-JUNCTION FIELDS
   Exit_X_1_Street       : Integer  := 21;
   Exit_X_1_Direction    : Cardinal := NORTH;
   Exit_X_2_Street       : Integer  := 22;
   Exit_X_2_Direction    : Cardinal := SOUTH;
   Exit_X_3_Street       : Integer  := 23;
   Exit_X_3_Direction    : Cardinal := EAST;
   Exit_X_4_Street       : Integer  := 24;
   Exit_X_4_Direction    : Cardinal := WEST;

   -----------------------------------------------------
   --                   SCAFFOLDING
   -----------------------------------------------------

   procedure Set_Up (T : in out Config_Reader_Test) is
   begin
      F_Reader :=
         Shared.Reader.Reference (Shared_Reader_Mock.Create);
      H_Reader :=
         Host_Config_Reader.Reference (Host_Config_Reader_Mock.Create);
      S_Reader :=
         Street_Config_Reader.Reference (Street_Config_Reader_Mock.Create);
      I_Reader := Intersection_Config_Reader.Reference (
                     Intersection_Config_Reader_Mock.Create);
      C_Reader := Config_Reader.Get_Instance (
         File_Reader         => F_Reader,
         Host_Reader         => H_Reader,
         Street_Reader       => S_Reader,
         Intersection_Reader => I_Reader);
   end Set_Up;

   function Get_JSON_For_T_Junction return G_JSON.JSON_Value is
      T_Junction : G_JSON.JSON_Value := G_JSON.Create_Object;
      Exits      : G_JSON.JSON_Array := G_JSON.Empty_Array;
      Exit_1     : G_JSON.JSON_Value := G_JSON.Create_Object;
      Exit_2     : G_JSON.JSON_Value := G_JSON.Create_Object;
      Exit_3     : G_JSON.JSON_Value := G_JSON.Create_Object;
   begin

      Exit_1.Set_Field ("streetId", Exit_T_1_Street);
      Exit_1.Set_Field ("direction", Cardinal'Image (Exit_T_1_Direction));
      Exit_2.Set_Field ("streetId", Exit_T_2_Street);
      Exit_2.Set_Field ("direction", Cardinal'Image (Exit_T_2_Direction));
      Exit_3.Set_Field ("streetId", Exit_T_3_Street);
      Exit_3.Set_Field ("direction", Cardinal'Image (Exit_T_3_Direction));

      G_JSON.Append (Exits, Exit_1);
      G_JSON.Append (Exits, Exit_2);
      G_JSON.Append (Exits, Exit_3);
      T_Junction.Set_Field ("exits", Exits);

      return T_Junction;
   end Get_JSON_For_T_Junction;

   function Get_JSON_For_X_Junction return G_JSON.JSON_Value is
      X_Junction : G_JSON.JSON_Value := G_JSON.Create_Object;
      Exits      : G_JSON.JSON_Array := G_JSON.Empty_Array;
      Exit_1     : G_JSON.JSON_Value := G_JSON.Create_Object;
      Exit_2     : G_JSON.JSON_Value := G_JSON.Create_Object;
      Exit_3     : G_JSON.JSON_Value := G_JSON.Create_Object;
      Exit_4     : G_JSON.JSON_Value := G_JSON.Create_Object;
   begin

      Exit_1.Set_Field ("streetId", Exit_X_1_Street);
      Exit_1.Set_Field ("direction", Cardinal'Image (Exit_X_1_Direction));
      Exit_2.Set_Field ("streetId", Exit_X_2_Street);
      Exit_2.Set_Field ("direction", Cardinal'Image (Exit_X_2_Direction));
      Exit_3.Set_Field ("streetId", Exit_X_3_Street);
      Exit_3.Set_Field ("direction", Cardinal'Image (Exit_X_3_Direction));
      Exit_4.Set_Field ("streetId", Exit_X_4_Street);
      Exit_4.Set_Field ("direction", Cardinal'Image (Exit_X_4_Direction));

      G_JSON.Append (Exits, Exit_1);
      G_JSON.Append (Exits, Exit_2);
      G_JSON.Append (Exits, Exit_3);
      G_JSON.Append (Exits, Exit_4);
      X_Junction.Set_Field ("exits", Exits);

      return X_Junction;
   end Get_JSON_For_X_Junction;

   -- Test Routines:
   procedure Test_Read_Hosts (T: in out TC.Test_Case'Class) is
      Hosts_JSON  : G_JSON.JSON_Array := G_JSON.Empty_Array;
      H_Reader_Mock : Host_Config_Reader_Mock.Reference
         := Host_Config_Reader_Mock.Reference (H_Reader);
      Host1_Json  : G_JSON.JSON_Value := G_JSON.Create_Object;
      Host1_Id    : Infra_Id := 72;
      Host2_Json  : G_JSON.JSON_Value := G_JSON.Create_Object;
      Host2_Id    : Infra_Id := 73;
      Result        : Infra_Id_List.List;
   begin
      null;
      H_Reader_Mock.Set_Return_Value_For_Read (Host1_Id);
      H_Reader_Mock.Set_Return_Value_For_Read (Host2_Id);

      Host1_Json.Set_Field ("id", Integer (Host1_Id));
      Host2_Json.Set_Field ("id", Integer (Host2_Id));
      G_JSON.Append (Hosts_JSON, Host1_Json);
      G_JSON.Append (Hosts_JSON, Host2_Json);

      Result := C_Reader.Read_Hosts (Hosts_JSON);

      Ass.Assert (H_Reader_Mock.Get_Set_Builder_Called,
                  "Builder was not set for the host config reader");
      Ass.Assert (Result.Contains (Host1_Id),
                  "Host 1 was not read by the config reader");
      Ass.Assert (Result.Contains (Host2_Id),
                  "Host 2 was not read by the config reader");
   end Test_Read_Hosts;

   procedure Test_Read_Streets (T: in out TC.Test_Case'Class) is
      Streets_JSON  : G_JSON.JSON_Array := G_JSON.Empty_Array;
      S_Reader_Mock : Street_Config_Reader_Mock.Reference
         := Street_Config_Reader_Mock.Reference (S_Reader);
      Street1_Json  : G_JSON.JSON_Value := G_JSON.Create_Object;
      Street1_Id    : Infra_Id := 42;
      Street2_Json  : G_JSON.JSON_Value := G_JSON.Create_Object;
      Street2_Id    : Infra_Id := 43;
      Result        : Infra_Id_List.List;
   begin
      S_Reader_Mock.Set_Return_Value_For_Read (Street1_Id);
      S_Reader_Mock.Set_Return_Value_For_Read (Street2_Id);

      Street1_Json.Set_Field ("id", Integer (Street1_Id));
      Street2_Json.Set_Field ("id", Integer (Street2_Id));
      G_JSON.Append (Streets_JSON, Street1_Json);
      G_JSON.Append (Streets_JSON, Street2_Json);

      Result := C_Reader.Read_Streets (Streets_JSON);

      Ass.Assert (S_Reader_Mock.Get_Set_Builder_Called,
                  "Builder was not set for the street config reader");
      Ass.Assert (Result.Contains (Street1_Id),
                  "Street 1 was not read by the config reader");
      Ass.Assert (Result.Contains (Street2_Id),
                  "Street 2 was not read by the config reader");
   end Test_Read_Streets;

   procedure Test_Read_Intersections (T: in out TC.Test_Case'Class) is
      Intersections_JSON    : G_JSON.JSON_Array := G_JSON.Empty_Array;
      I_Reader_Mock         : Intersection_Config_Reader_Mock.Reference
         := Intersection_Config_Reader_Mock.Reference (I_Reader);
      Intersection1_Json    : G_JSON.JSON_Value := G_JSON.Create_Object;
      Intersection1_Id      : Infra_Id := 42;
      Intersection2_Json    : G_JSON.JSON_Value := G_JSON.Create_Object;
      Intersection2_Id      : Infra_Id := 43;
      Result                : Infra_Id_List.List;
   begin

      I_Reader_Mock.Set_Return_Value_For_Read (Intersection1_Id);
      I_Reader_Mock.Set_Return_Value_For_Read (Intersection2_Id);

      Intersection1_Json := Get_JSON_For_T_Junction;
      Intersection2_Json := Get_JSON_For_X_Junction;

      Intersection1_Json.Set_Field ("id", Integer (Intersection1_Id));
      Intersection2_Json.Set_Field ("id", Integer (Intersection2_Id));

      G_JSON.Append (Intersections_JSON, Intersection1_Json);
      G_JSON.Append (Intersections_JSON, Intersection2_Json);

      Result := C_Reader.Read_Intersections (Intersections_JSON);

      Ass.Assert (I_Reader_Mock.Get_Set_Builder_Called,
                  "Builder was not set for the Intersection config reader");
      Ass.Assert (I_Reader_Mock.Get_Read_Called,
                  "Read operation of reader was not called");
      Ass.Assert (Result.Contains (Intersection1_Id),
                  "Intersection 1 was not read by the config reader");
      Ass.Assert (Result.Contains (Intersection2_Id),
                  "Intersection 2 was not read by the config reader");
      Ass.Assert (Integer (Result.Length) = 2,
                  "Intersection 2 was not read by the config reader");
   end Test_Read_Intersections;

-- TODO: Put something in facilities array
   procedure Test_Read_Config (T: in out TC.Test_Case'Class)
   is
      Hosts_JSON    : G_JSON.JSON_Array := G_JSON.Empty_Array;
      Streets_JSON  : G_JSON.JSON_Array := G_JSON.Empty_Array;
      Intersections_JSON    : G_JSON.JSON_Array := G_JSON.Empty_Array;
      District_Str  : String            := "district.cfg";
      District_JSON : G_JSON.JSON_Value := G_JSON.Create_Object;
      F_Reader_Mock : Shared_Reader_Mock.Reference
         := Shared_Reader_Mock.Reference (F_Reader);
      S_Reader_Mock : Street_Config_Reader_Mock.Reference
         := Street_Config_Reader_Mock.Reference (S_Reader);
      I_Reader_Mock : Intersection_Config_Reader_Mock.Reference
         := Intersection_Config_Reader_Mock.Reference (I_Reader);
      Street1_Json  : G_JSON.JSON_Value := G_JSON.Create_Object;
      Street1_Id    : Infra_Id          := 42;
      Street2_Json  : G_JSON.JSON_Value := G_JSON.Create_Object;
      Street2_Id    : Infra_Id          := 43;
      Intersection1_Json  : G_JSON.JSON_Value := G_JSON.Create_Object;
      Intersection1_Id    : Infra_Id := 42;
      Intersection1_Exits : G_JSON.JSON_Array := G_JSON.Empty_Array;
      Intersection2_Json  : G_JSON.JSON_Value := G_JSON.Create_Object;
      Intersection2_Id    : Infra_Id := 43;
      Intersection2_Exits : G_JSON.JSON_Array := G_JSON.Empty_Array;
      Outcome       : Boolean;
   begin
      S_Reader_Mock.Set_Return_Value_For_Read (Street1_Id);
      S_Reader_Mock.Set_Return_Value_For_Read (Street2_Id);

      Street1_Json.Set_Field ("id", Integer (Street1_Id));
      Street2_Json.Set_Field ("id", Integer (Street2_Id));
      G_JSON.Append (Streets_JSON, Street1_Json);
      G_JSON.Append (Streets_JSON, Street2_Json);

      I_Reader_Mock.Set_Return_Value_For_Read (Intersection1_Id);
      I_Reader_Mock.Set_Return_Value_For_Read (Intersection2_Id);

      Intersection1_Json := Get_JSON_For_T_Junction;
      Intersection2_Json := Get_JSON_For_X_Junction;
      Intersection1_Json.Set_Field ("id", Integer (Intersection1_Id));
      Intersection2_Json.Set_Field ("id", Integer (Intersection2_Id));

      G_JSON.Append (Intersections_JSON, Intersection1_Json);
      G_JSON.Append (Intersections_JSON, Intersection2_Json);

      District_JSON.Set_Field ("facilities", Hosts_JSON);
      District_JSON.Set_Field ("streets", Streets_JSON);
      District_JSON.Set_Field ("intersections", Intersections_JSON);

      F_Reader_Mock.Set_Return_Value_For_Parse (District_JSON);

      Outcome := C_Reader.Read_Config (District_Str);

      Ass.Assert (S_Reader_Mock.Get_Set_Builder_Called,
                  "Builder was not set for the street config reader");
      Ass.Assert (Outcome,
                  "The read config procedure did not went well");
   end Test_Read_Config;

   procedure Register_Tests (T : in out Config_Reader_Test) is
      use TC.Registration;
   begin

      Register_Routine (Test    => T,
                        Routine => Test_Read_Hosts'Access,
                        Name    => "A config reader can read hosts");

      Register_Routine (Test    => T,
                        Routine => Test_Read_Streets'Access,
                        Name    => "A config reader can read streets");

      Register_Routine (Test    => T,
                        Routine => Test_Read_Intersections'Access,
                        Name    => "A config reader can read intersections");

      Register_Routine (Test    => T,
                        Routine => Test_Read_Config'Access,
                        Name    => "A config reader can read config files");

   end Register_Tests;

   function Name (T : in Config_Reader_Test)
      return AU.Message_String is
   begin
      return AU.Format ("Config_Reader");
   end Name;

end Reactive.Infrastructure.Build.Config_Reader.Tests;
