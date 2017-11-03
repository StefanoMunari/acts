with AUnit.Assertions;
with Ada.Text_IO;

with Active.Agent;

with Reactive.District.Mock;
with Reactive.Infrastructure.Mock;
with Reactive.Infrastructure.Way.Roadway.Mock;

with Shared.Infra_Id_Set;

-- TODO: Look for Add_Infrastructure and add something else
package body Reactive.Infrastructure.Utils.Tests is
   package Ass renames AUnit.Assertions;
   package Agent renames Active.Agent;
   package Infra_Id_Set renames Shared.Infra_Id_Set;
   package District_Mock renames Reactive.District.Mock;
   package Infrastructure_Mock renames Reactive.Infrastructure.Mock;
   package Roadway_Mock renames Reactive.Infrastructure.Way.Roadway.Mock;

   -----------------------------------------------------
   --                   SCAFFOLDING
   -----------------------------------------------------
   Infrastructure_Utils : Reactive.Infrastructure.Utils.Reference;
   District : access District_Mock.Object;

   procedure Set_Up (T: in out Infrastructure_Utils_Test) is
   begin
      District := District_Mock.Create;

      Infrastructure_Utils
        := Reactive.Infrastructure.Utils
          .Get_Instance (District => District);
   end Set_Up;

   procedure Test_Exists (T: in out TC.Test_Case'Class)
   is
      Roadway_Mock_Ref : Roadway_Mock.Reference := Roadway_Mock.Create;
      Roadway : aliased Reactive.Infrastructure.Way.Roadway.Reference
        := Reactive.Infrastructure.Way.Roadway.Reference (Roadway_Mock_Ref);
      Roadway_Id : Infra_Id := 345;
      Added : Boolean := FALSE;
   begin
      Roadway_Mock_Ref.Set_Id (Roadway_Id);
      District.Add_Roadway (Infrastructure => Roadway,
                            Added          => Added);

      Ass.Assert (Added,
                  "The infrastructure is not added to district");

      Ass.Assert (Infrastructure_Utils.Exists (Roadway_Id),
                  "The infrastructure not exists");
   end Test_Exists;

   procedure Test_Not_Exists (T: in out TC.Test_Case'Class)
   is
      Infrastructure_Id : Infra_Id := 345;
   begin
      Ass.Assert (not Infrastructure_Utils.Exists (Infrastructure_Id),
                  "The infrastructure not exists");
   end Test_Not_Exists;

   -----------------------------------------------------
   --                  REGISTRATION
   -----------------------------------------------------
   procedure Register_Tests (T: in out Infrastructure_Utils_Test) is
      use TC.Registration;
   begin

      Register_Routine (Test    => T,
                        Routine => Test_Exists'Access,
                        Name    => "Test exists");

      Register_Routine (Test    => T,
                        Routine => Test_Not_Exists'Access,
                        Name    => "Test not exists");

   end Register_Tests;

   function Name(T: Infrastructure_Utils_Test) return AU.Message_String is
   begin
      return AU.Format ("Infrastructure_Utils");
   end Name;
end Reactive.Infrastructure.Utils.Tests;
