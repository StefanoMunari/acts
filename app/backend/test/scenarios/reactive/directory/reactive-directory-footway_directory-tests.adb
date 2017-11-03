with AUnit.Assertions;
with Reactive.Infrastructure.Way.Footway.Mock;

package body Reactive.Directory.Footway_Directory.Tests is
   package Ass renames AUnit.Assertions;
   package Footway_Mock renames  Reactive.Infrastructure.Way.Footway.Mock;

   -----------------------------------------------------
   --                   SCAFFOLDING
   -----------------------------------------------------
   Footway_Directory_Obj : Reactive.Directory.Footway_Directory.Directory;

   procedure Set_Up (T: in out Footway_Directory_Test) is
   begin
      null;
   end Set_Up;

   procedure Tear_Down (T: in out Footway_Directory_Test) is
   begin
      Footway_Directory_Obj.Clear;
   end Tear_Down;

   -----------------------------------------------------
   --                   SCENARIOS
   -----------------------------------------------------

   procedure Test_Add (T : in out TC.Test_Case'Class)
   is
      Footway_Id : Infra_Id := 42;
      Footway : access Footway_Mock.Object'Class
        := Footway_Mock.Create;
      Footway_Ref : aliased Reactive.Infrastructure.Way.Footway.Reference
         := Reactive.Infrastructure.Way.Footway.Reference (Footway);
      Added : Boolean := FALSE;
   begin
      Footway.Set_Id (Footway_Id);

      Ass.Assert (not Footway_Directory_Obj
                  .Contains_Infrastructure (Footway_Id),
                  "The Footway directory already contains the footway");

      Footway_Directory_Obj.Add (
         Infrastructure => Footway_Ref,
         Added          => Added);

      Ass.Assert (Footway_Directory_Obj
                  .Contains_Infrastructure (Footway_Id),
                  "The Footway directory does not "
                  & "contains the footway id");

      Ass.Assert (Added = TRUE,
                  "Added flag should have been set to true");
   end Test_Add;

   procedure Test_Contains_Infrastructure (T : in out TC.Test_Case'Class)
   is
      Footway_Id : Infra_Id := 63;
      Footway : access Footway_Mock.Object'Class
        := Footway_Mock.Create;
      Footway_Ref : aliased Reactive.Infrastructure.Way.Footway.Reference
         := Reactive.Infrastructure.Way.Footway.Reference (Footway);
      Added : Boolean := FALSE;
   begin
      Footway.Set_Id (Footway_Id);

      Ass.Assert (not Footway_Directory_Obj
                  .Contains_Infrastructure (Footway_Id),
                  "The Footway directory already contains the footway");

      Footway_Directory_Obj.Add (
         Infrastructure => Footway_Ref,
         Added          => Added);

      Ass.Assert (Footway_Directory_Obj
                  .Contains_Infrastructure (Footway_Id),
                  "The Footway directory does not "
                  & "contains the footway id");
   end Test_Contains_Infrastructure;

   procedure Test_Find_By_Id (T : in out TC.Test_Case'Class)
   is
      Footway_Id : Infra_Id := 27;
      Footway : access Footway_Mock.Object'Class
        := Footway_Mock.Create;
      Footway_Ref : aliased Reactive.Infrastructure.Way.Footway.Reference
         := Reactive.Infrastructure.Way.Footway.Reference (Footway);
      Found_Footway : access Reactive.Infrastructure.Way.Footway.Object'Class;
      Added : Boolean := FALSE;
   begin
      Footway.Set_Id (Footway_Id);

      Ass.Assert (not Footway_Directory_Obj
                  .Contains_Infrastructure (Footway_Id),
                  "The Footway directory already contains the footway");

      Footway_Directory_Obj.Add (
         Infrastructure => Footway_Ref,
         Added          => Added);

      Found_Footway := Footway_Directory_Obj.Find_By_Id (Footway_Id);

      Ass.Assert (Found_Footway = Footway_Ref,
                  "The Footway directory does not return "
                  & "the right footway");
   end Test_Find_By_Id;

   procedure Test_Safe_Find_By_Id (T : in out TC.Test_Case'Class)
   is
      Footway_Id : Infra_Id := 27;
      Footway : access Footway_Mock.Object'Class
        := Footway_Mock.Create;
      Footway_Ref : aliased Reactive.Infrastructure.Way.Footway.Reference
         := Reactive.Infrastructure.Way.Footway.Reference (Footway);
      Found_Footway : access Reactive.Infrastructure.Way.Footway.Object'Class;
      Added : Boolean := FALSE;
      Found : Boolean := FALSE;
   begin
      Footway.Set_Id (Footway_Id);

      Ass.Assert (not Footway_Directory_Obj
                  .Contains_Infrastructure (Footway_Id),
                  "The Footway directory already contains the Footway");

      Footway_Directory_Obj.Add (
         Infrastructure => Footway_Ref,
         Added          => Added);

      Found_Footway := Footway_Directory_Obj.Safe_Find_By_Id (Footway_Id,
         Found);

      Ass.Assert (Found,
                  "Find operation was not successful");

      Ass.Assert (Found_Footway = Footway_Ref,
                  "The Footway directory does not return "
                  & "the right Footway");
   end Test_Safe_Find_By_Id;

   -----------------------------------------------------
   --                  REGISTRATION
   -----------------------------------------------------
   procedure Register_Tests (T: in out Footway_Directory_Test) is
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

   function Name(T: Footway_Directory_Test) return AU.Message_String is
   begin
      return AU.Format ("Footway_Directory");
   end Name;
end Reactive.Directory.Footway_Directory.Tests;
