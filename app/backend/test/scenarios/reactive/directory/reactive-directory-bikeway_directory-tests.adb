with AUnit.Assertions;
with Reactive.Infrastructure.Way.Bikeway.Mock;

package body Reactive.Directory.Bikeway_Directory.Tests is
   package Ass renames AUnit.Assertions;
   package Bikeway_Mock renames  Reactive.Infrastructure.Way.Bikeway.Mock;

   -----------------------------------------------------
   --                   SCAFFOLDING
   -----------------------------------------------------
   Bikeway_Directory_Obj : Reactive.Directory.Bikeway_Directory.Directory;

   procedure Set_Up (T: in out Bikeway_Directory_Test) is
   begin
      null;
   end Set_Up;

   procedure Tear_Down (T: in out Bikeway_Directory_Test) is
   begin
      Bikeway_Directory_Obj.Clear;
   end Tear_Down;

   -----------------------------------------------------
   --                   SCENARIOS
   -----------------------------------------------------

   procedure Test_Add (T : in out TC.Test_Case'Class)
   is
      Bikeway_Id : Infra_Id := 42;
      Bikeway : access Bikeway_Mock.Object'Class
        := Bikeway_Mock.Create;
      Bikeway_Ref : aliased Reactive.Infrastructure.Way.Bikeway.Reference
         := Reactive.Infrastructure.Way.Bikeway.Reference (Bikeway);
      Added : Boolean := FALSE;
   begin
      Bikeway.Set_Id (Bikeway_Id);

      Ass.Assert (not Bikeway_Directory_Obj
                  .Contains_Infrastructure (Bikeway_Id),
                  "The Bikeway directory already contains the Bikeway");

      Bikeway_Directory_Obj.Add (
         Infrastructure => Bikeway_Ref,
         Added     => Added);

      Ass.Assert (Bikeway_Directory_Obj
                  .Contains_Infrastructure (Bikeway_Id),
                  "The Bikeway directory does not "
                  & "contains the Bikeway id");

      Ass.Assert (Added = TRUE,
                  "Added flag should have been set to true");
   end Test_Add;

   procedure Test_Contains_Infrastructure (T : in out TC.Test_Case'Class)
   is
      Bikeway_Id : Infra_Id := 63;
      Bikeway : access Bikeway_Mock.Object'Class
        := Bikeway_Mock.Create;
      Bikeway_Ref : aliased Reactive.Infrastructure.Way.Bikeway.Reference
         := Reactive.Infrastructure.Way.Bikeway.Reference (Bikeway);
      Added : Boolean := FALSE;
   begin
      Bikeway.Set_Id (Bikeway_Id);

      Ass.Assert (not Bikeway_Directory_Obj
                  .Contains_Infrastructure (Bikeway_Id),
                  "The Bikeway directory already contains the Bikeway");

      Bikeway_Directory_Obj.Add (
         Infrastructure => Bikeway_Ref,
         Added          => Added);

      Ass.Assert (Bikeway_Directory_Obj
                  .Contains_Infrastructure (Bikeway_Id),
                  "The Bikeway directory does not "
                  & "contains the Bikeway id");
   end Test_Contains_Infrastructure;

   procedure Test_Find_By_Id (T : in out TC.Test_Case'Class)
   is
      Bikeway_Id : Infra_Id := 27;
      Bikeway : access Bikeway_Mock.Object'Class
        := Bikeway_Mock.Create;
      Bikeway_Ref : aliased Reactive.Infrastructure.Way.Bikeway.Reference
         := Reactive.Infrastructure.Way.Bikeway.Reference (Bikeway);
      Found_Bikeway : access Reactive.Infrastructure.Way.Bikeway.Object'Class;
      Added : Boolean := FALSE;
   begin
      Bikeway.Set_Id (Bikeway_Id);

      Ass.Assert (not Bikeway_Directory_Obj
                  .Contains_Infrastructure (Bikeway_Id),
                  "The Bikeway directory already contains the Bikeway");

      Bikeway_Directory_Obj.Add (
         Infrastructure => Bikeway_Ref,
         Added          => Added);

      Found_Bikeway := Bikeway_Directory_Obj.Find_By_Id (Bikeway_Id);

      Ass.Assert (Found_Bikeway = Bikeway_Ref,
                  "The Bikeway directory does not return "
                  & "the right Bikeway");
   end Test_Find_By_Id;

   procedure Test_Safe_Find_By_Id (T : in out TC.Test_Case'Class)
   is
      Bikeway_Id : Infra_Id := 27;
      Bikeway : access Bikeway_Mock.Object'Class
        := Bikeway_Mock.Create;
      Bikeway_Ref : aliased Reactive.Infrastructure.Way.Bikeway.Reference
         := Reactive.Infrastructure.Way.Bikeway.Reference (Bikeway);
      Found_Bikeway : access Reactive.Infrastructure.Way.Bikeway.Object'Class;
      Added : Boolean := FALSE;
      Found : Boolean := FALSE;
   begin
      Bikeway.Set_Id (Bikeway_Id);

      Ass.Assert (not Bikeway_Directory_Obj
                  .Contains_Infrastructure (Bikeway_Id),
                  "The Bikeway directory already contains the Bikeway");

      Bikeway_Directory_Obj.Add (
         Infrastructure => Bikeway_Ref,
         Added          => Added);

      Found_Bikeway := Bikeway_Directory_Obj.Safe_Find_By_Id (Bikeway_Id,
         Found);

      Ass.Assert (Found,
                  "Find operation was not successful");

      Ass.Assert (Found_Bikeway = Bikeway_Ref,
                  "The Bikeway directory does not return "
                  & "the right Bikeway");
   end Test_Safe_Find_By_Id;

   -----------------------------------------------------
   --                  REGISTRATION
   -----------------------------------------------------
   procedure Register_Tests (T: in out Bikeway_Directory_Test) is
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

   function Name(T: Bikeway_Directory_Test) return AU.Message_String is
   begin
      return AU.Format ("Bikeway_Directory");
   end Name;
end Reactive.Directory.Bikeway_Directory.Tests;
