with AUnit.Assertions;
with Reactive.Infrastructure.Way.Roadway.Mock;

package body Reactive.Directory.Roadway_Directory.Tests is
   package Ass renames AUnit.Assertions;
   package Roadway_Mock renames  Reactive.Infrastructure.Way.Roadway.Mock;

   -----------------------------------------------------
   --                   SCAFFOLDING
   -----------------------------------------------------
   Roadway_Directory_Obj : Reactive.Directory.Roadway_Directory.Directory;

   procedure Set_Up (T: in out Roadway_Directory_Test) is
   begin
      null;
   end Set_Up;

   procedure Tear_Down (T: in out Roadway_Directory_Test) is
   begin
      Roadway_Directory_Obj.Clear;
   end Tear_Down;

   -----------------------------------------------------
   --                   SCENARIOS
   -----------------------------------------------------

   procedure Test_Add (T : in out TC.Test_Case'Class)
   is
      Roadway_Id : Infra_Id := 42;
      Roadway : access Roadway_Mock.Object'Class
        := Roadway_Mock.Create;
      Roadway_Ref : aliased Reactive.Infrastructure.Way.Roadway.Reference
         := Reactive.Infrastructure.Way.Roadway.Reference (Roadway);
      Added : Boolean := FALSE;
   begin
      Roadway.Set_Id (Roadway_Id);

      Ass.Assert (not Roadway_Directory_Obj
                  .Contains_Infrastructure (Roadway_Id),
                  "The Roadway directory already contains the Roadway");

      Roadway_Directory_Obj.Add (
         Infrastructure => Roadway_Ref,
         Added          => Added);

      Ass.Assert (Roadway_Directory_Obj
                  .Contains_Infrastructure (Roadway_Id),
                  "The Roadway directory does not "
                  & "contains the Roadway id");

      Ass.Assert (Added = TRUE,
                  "Added flag should have been set to true");
   end Test_Add;

   procedure Test_Contains_Infrastructure (T : in out TC.Test_Case'Class)
   is
      Roadway_Id : Infra_Id := 63;
      Roadway : access Roadway_Mock.Object'Class
        := Roadway_Mock.Create;
      Roadway_Ref : aliased Reactive.Infrastructure.Way.Roadway.Reference
         := Reactive.Infrastructure.Way.Roadway.Reference (Roadway);
      Added : Boolean := FALSE;
   begin
      Roadway.Set_Id (Roadway_Id);

      Ass.Assert (not Roadway_Directory_Obj
                  .Contains_Infrastructure (Roadway_Id),
                  "The Roadway directory already contains the Roadway");

      Roadway_Directory_Obj.Add (
         Infrastructure => Roadway_Ref,
         Added          => Added);

      Ass.Assert (Roadway_Directory_Obj
                  .Contains_Infrastructure (Roadway_Id),
                  "The Roadway directory does not "
                  & "contains the Roadway id");
   end Test_Contains_Infrastructure;

   procedure Test_Find_By_Id (T : in out TC.Test_Case'Class)
   is
      Roadway_Id : Infra_Id := 27;
      Roadway : access Roadway_Mock.Object'Class
        := Roadway_Mock.Create;
      Roadway_Ref : aliased Reactive.Infrastructure.Way.Roadway.Reference
         := Reactive.Infrastructure.Way.Roadway.Reference (Roadway);
      Found_Roadway : access Reactive.Infrastructure.Way.Roadway.Object'Class;
      Added : Boolean := FALSE;
   begin
      Roadway.Set_Id (Roadway_Id);

      Ass.Assert (not Roadway_Directory_Obj
                  .Contains_Infrastructure (Roadway_Id),
                  "The Roadway directory already contains the Roadway");

      Roadway_Directory_Obj.Add (
         Infrastructure => Roadway_Ref,
         Added          => Added);

      Found_Roadway := Roadway_Directory_Obj.Find_By_Id (Roadway_Id);

      Ass.Assert (Found_Roadway = Roadway_Ref,
                  "The Roadway directory does not return "
                  & "the right Roadway");
   end Test_Find_By_Id;

   procedure Test_Safe_Find_By_Id (T : in out TC.Test_Case'Class)
   is
      Roadway_Id : Infra_Id := 27;
      Roadway : access Roadway_Mock.Object'Class
        := Roadway_Mock.Create;
      Roadway_Ref : aliased Reactive.Infrastructure.Way.Roadway.Reference
         := Reactive.Infrastructure.Way.Roadway.Reference (Roadway);
      Found_Roadway : access Reactive.Infrastructure.Way.Roadway.Object'Class;
      Added : Boolean := FALSE;
      Found : Boolean := FALSE;
   begin
      Roadway.Set_Id (Roadway_Id);

      Ass.Assert (not Roadway_Directory_Obj
                  .Contains_Infrastructure (Roadway_Id),
                  "The Roadway directory already contains the Roadway");

      Roadway_Directory_Obj.Add (
         Infrastructure => Roadway_Ref,
         Added          => Added);

      Found_Roadway := Roadway_Directory_Obj.Safe_Find_By_Id (Roadway_Id,
         Found);

      Ass.Assert (Found,
                  "Find operation was not successful");

      Ass.Assert (Found_Roadway = Roadway_Ref,
                  "The Roadway directory does not return "
                  & "the right Roadway");
   end Test_Safe_Find_By_Id;

   -----------------------------------------------------
   --                  REGISTRATION
   -----------------------------------------------------
   procedure Register_Tests (T: in out Roadway_Directory_Test) is
      use TC.Registration;
   begin
      Register_Routine (Test => T,
                        Routine => Test_Add'Access,
                        Name => "Test add");
      Register_Routine (Test => T,
                        Routine => Test_Contains_Infrastructure'Access,
                        Name => "Test contains infrastructure");
      Register_Routine (Test => T,
                        Routine => Test_Find_By_Id'Access,
                        Name => "Test find by id");
      Register_Routine (Test => T,
                        Routine => Test_Safe_Find_By_Id'Access,
                        Name => "Test safe find by id");
   end Register_Tests;

   function Name(T: Roadway_Directory_Test) return AU.Message_String is
   begin
      return AU.Format ("Roadway_Directory");
   end Name;
end Reactive.Directory.Roadway_Directory.Tests;
