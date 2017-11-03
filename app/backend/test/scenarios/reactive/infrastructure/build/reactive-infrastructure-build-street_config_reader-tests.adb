with AUnit.Assertions;

-- gnatcoll libs
with GNATCOLL.JSON;

with Reactive.Infrastructure.Build.Street_Builder.Mock;

package body Reactive.Infrastructure.Build.Street_Config_Reader.Tests is
   package Ass    renames AUnit.Assertions;
   package G_JSON renames GNATCOLL.JSON;
   package Street_Builder
      renames Reactive.Infrastructure.Build.Street_Builder;
   package Street_Builder_Mock
      renames Reactive.Infrastructure.Build.Street_Builder.Mock;
   use Reactive.Infra_Id_Type;
   use Street_Builder;

   -----------------------------------------------------
   --                   SCAFFOLDING
   -----------------------------------------------------

   SCR     : Street_Config_Reader.Reference;
   Builder : Street_Builder.Reference;

   procedure Set_Up (T : in out Street_Config_Reader_Test) is
   begin
      Builder := Street_Builder.Reference (Street_Builder_Mock.Create);
      SCR     := new Street_Config_Reader.Object;
   end Set_Up;

   -- Test Routines:
   procedure Test_Set_Builder (T: in out TC.Test_Case'Class) is
   begin
      SCR.Set_Builder (Builder);

      Ass.Assert (Builder = SCR.Builder,
                 "Builder was not set correctly");
   end Test_Set_Builder;

   procedure Test_Read (T: in out TC.Test_Case'Class) is
      Builder_Mock : Street_Builder_Mock.Reference :=
         Street_Builder_Mock.Reference (Builder);
      Street_Json    : G_JSON.JSON_Value := G_JSON.Create_Object;
      Street_Id      : Infra_Id := 42;
      Bikeway_Id     : Infra_Id := 51;
      Bikeway_Value  : G_JSON.JSON_Value := G_JSON.Create_Object;
      Bikeways_Array : G_JSON.JSON_Array := G_JSON.Empty_Array;
      Footway_Id     : Infra_Id := 52;
      Footway_Value  : G_JSON.JSON_Value := G_JSON.Create_Object;
      Footways_Array : G_JSON.JSON_Array := G_JSON.Empty_Array;
      Roadway_Id     : Infra_Id := 53;
      Roadway_Value  : G_JSON.JSON_Value := G_JSON.Create_Object;
      Roadways_Array : G_JSON.JSON_Array := G_JSON.Empty_Array;
      Result         : Infra_Id;
   begin
      Builder_Mock.Set_Return_Value_For_With_Bikeway (Bikeway_Id);
      Builder_Mock.Set_Return_Value_For_With_Footway (Footway_Id);
      Builder_Mock.Set_Return_Value_For_With_Roadway (Roadway_Id);
      Builder_Mock.Set_Return_Value_For_Get_Street (Street_Id);

      SCR.Set_Builder (Builder);

      Bikeway_Value.Set_Field ("id", Integer (Bikeway_Id));
      G_JSON.Append (Bikeways_Array, Bikeway_Value);
      Footway_Value.Set_Field ("id", Integer (Footway_Id));
      G_JSON.Append (Footways_Array, Footway_Value);
      Roadway_Value.Set_Field ("id", Integer (Roadway_Id));
      G_JSON.Append (Roadways_Array, Roadway_Value);

      Street_Json.Set_Field ("bikeways", Bikeways_Array);
      Street_Json.Set_Field ("footways", Footways_Array);
      Street_Json.Set_Field ("roadways", Roadways_Array);
      Street_Json.Set_Field ("id", Integer (Street_Id));

      Result := SCR.Read (Street_Json);

      Ass.Assert (Builder_Mock.Get_With_Bikeway_Called,
                 "Bikeway was not build");
      Ass.Assert (Builder_Mock.Get_With_Footway_Called,
                 "Footway was not build");
      Ass.Assert (Builder_Mock.Get_With_Roadway_Called,
                 "Roadway was not build");
      Ass.Assert (Street_Id = Result,
                 "Street id was not read correctly");
   end Test_Read;

   procedure Register_Tests (T : in out Street_Config_Reader_Test) is
      use TC.Registration;
   begin

      Register_Routine (Test    => T,
                        Routine => Test_Set_Builder'Access,
                        Name    => "A builder can be set for the SCR");

      Register_Routine (Test    => T,
                        Routine => Test_Read'Access,
                        Name    => "The SCR can read the street config");

   end Register_Tests;

   function Name (T : in Street_Config_Reader_Test)
      return AU.Message_String is
   begin
      return AU.Format ("Street_Config_Reader");
   end Name;

end Reactive.Infrastructure.Build.Street_Config_Reader.Tests;
