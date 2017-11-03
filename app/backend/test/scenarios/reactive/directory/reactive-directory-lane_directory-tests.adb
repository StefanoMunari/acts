with AUnit.Assertions;
with Reactive.Infrastructure.Lane.Mock;

package body Reactive.Directory.Lane_Directory.Tests is
   package Ass renames AUnit.Assertions;
   package Lane_Mock renames  Reactive.Infrastructure.Lane.Mock;

   -----------------------------------------------------
   --                   SCAFFOLDING
   -----------------------------------------------------
   Lane_Directory_Obj : Reactive.Directory.Lane_Directory.Directory;

   procedure Set_Up (T: in out Lane_Directory_Test) is
   begin
      null;
   end Set_Up;

   procedure Tear_Down (T: in out Lane_Directory_Test) is
   begin
      Lane_Directory_Obj.Clear;
   end Tear_Down;

   -----------------------------------------------------
   --                   SCENARIOS
   -----------------------------------------------------

   procedure Test_Add (T : in out TC.Test_Case'Class)
   is
      Lane_Id : Infra_Id := 42;
      Lane : access Lane_Mock.Object'Class
        := Lane_Mock.Create;
      Lane_Ref : aliased Reactive.Infrastructure.Lane.Reference
         := Reactive.Infrastructure.Lane.Reference (Lane);
      Added : Boolean := FALSE;
   begin
      Lane.Set_Id (Lane_Id);

      Ass.Assert (not Lane_Directory_Obj
                  .Contains_Infrastructure (Lane_Id),
                  "The Lane directory already contains the Lane");

      Lane_Directory_Obj.Add
         (Infrastructure => Lane_Ref,
          Added          => Added);

      Ass.Assert (Lane_Directory_Obj
                  .Contains_Infrastructure (Lane_Id),
                  "The Lane directory does not "
                  & "contains the Lane id");

      Ass.Assert (Added = TRUE,
                  "Added flag should have been set to true");
   end Test_Add;

   procedure Test_Contains_Infrastructure (T : in out TC.Test_Case'Class)
   is
      Lane_Id : Infra_Id := 63;
      Lane : access Lane_Mock.Object'Class
        := Lane_Mock.Create;
      Lane_Ref : aliased Reactive.Infrastructure.Lane.Reference
         := Reactive.Infrastructure.Lane.Reference (Lane);
      Added : Boolean := FALSE;
   begin
      Lane.Set_Id (Lane_Id);

      Ass.Assert (not Lane_Directory_Obj
                  .Contains_Infrastructure (Lane_Id),
                  "The Lane directory already contains the Lane");

      Lane_Directory_Obj.Add
         (Infrastructure => Lane_Ref,
          Added          => Added);

      Ass.Assert (Lane_Directory_Obj
                  .Contains_Infrastructure (Lane_Id),
                  "The Lane directory does not "
                  & "contains the Lane id");
   end Test_Contains_Infrastructure;

   procedure Test_Find_By_Id (T : in out TC.Test_Case'Class)
   is
      Lane_Id : Infra_Id := 27;
      Lane : access Lane_Mock.Object'Class
        := Lane_Mock.Create;
      Lane_Ref : aliased Reactive.Infrastructure.Lane.Reference
         := Reactive.Infrastructure.Lane.Reference (Lane);
      Found_Lane : access Reactive.Infrastructure.Lane.Object'Class;
      Added : Boolean := FALSE;
   begin
      Lane.Set_Id (Lane_Id);

      Ass.Assert (not Lane_Directory_Obj
                  .Contains_Infrastructure (Lane_Id),
                  "The Lane directory already contains the Lane");

      Lane_Directory_Obj.Add
         (Infrastructure => Lane_Ref,
          Added          => Added);

      Found_Lane := Lane_Directory_Obj.Find_By_Id (Lane_Id);

      Ass.Assert (Found_Lane = Lane_Ref,
                  "The Lane directory does not return "
                  & "the right Lane");
   end Test_Find_By_Id;

   procedure Test_Safe_Find_By_Id (T : in out TC.Test_Case'Class)
   is
      Lane_Id : Infra_Id := 27;
      Lane : access Lane_Mock.Object'Class
        := Lane_Mock.Create;
      Lane_Ref : aliased Reactive.Infrastructure.Lane.Reference
         := Reactive.Infrastructure.Lane.Reference (Lane);
      Found_Lane : access Reactive.Infrastructure.Lane.Object'Class;
      Added : Boolean := FALSE;
      Found : Boolean := FALSE;
   begin
      Lane.Set_Id (Lane_Id);

      Ass.Assert (not Lane_Directory_Obj
                  .Contains_Infrastructure (Lane_Id),
                  "The Lane directory already contains the Lane");

      Lane_Directory_Obj.Add
         (Infrastructure => Lane_Ref,
          Added          => Added);

      Found_Lane := Lane_Directory_Obj.Safe_Find_By_Id (Lane_Id,
         Found);

      Ass.Assert (Found,
                  "Find operation was not successful");

      Ass.Assert (Found_Lane = Lane_Ref,
                  "The Lane directory does not return "
                  & "the right Lane");
   end Test_Safe_Find_By_Id;

   -----------------------------------------------------
   --                  REGISTRATION
   -----------------------------------------------------
   procedure Register_Tests (T: in out Lane_Directory_Test) is
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

   function Name(T: Lane_Directory_Test) return AU.Message_String is
   begin
      return AU.Format ("Lane_Directory");
   end Name;
end Reactive.Directory.Lane_Directory.Tests;
