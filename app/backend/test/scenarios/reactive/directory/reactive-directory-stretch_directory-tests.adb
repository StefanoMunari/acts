with AUnit.Assertions;
with Reactive.Infrastructure.Stretch.Mock;

package body Reactive.Directory.Stretch_Directory.Tests is
   package Ass renames AUnit.Assertions;
   package Stretch_Mock renames  Reactive.Infrastructure.Stretch.Mock;

   -----------------------------------------------------
   --                   SCAFFOLDING
   -----------------------------------------------------
   Stretch_Directory_Obj : Reactive.Directory.Stretch_Directory.Directory;

   procedure Set_Up (T: in out Stretch_Directory_Test) is
   begin
      null;
   end Set_Up;

   procedure Tear_Down (T: in out Stretch_Directory_Test) is
   begin
      Stretch_Directory_Obj.Clear;
   end Tear_Down;

   -----------------------------------------------------
   --                   SCENARIOS
   -----------------------------------------------------

   procedure Test_Add (T : in out TC.Test_Case'Class)
   is
      Stretch_Id : Infra_Id := 42;
      Stretch : access Stretch_Mock.Object'Class
        := Stretch_Mock.Create;
      Stretch_Ref : aliased Reactive.Infrastructure.Stretch.Reference
         := Reactive.Infrastructure.Stretch.Reference (Stretch);
      Added : Boolean := FALSE;
   begin
      Stretch.Set_Id (Stretch_Id);

      Ass.Assert (not Stretch_Directory_Obj
                  .Contains_Infrastructure (Stretch_Id),
                  "The Stretch directory already contains the Stretch");

      Stretch_Directory_Obj.Add
         (Infrastructure => Stretch_Ref,
          Added          => Added);

      Ass.Assert (Stretch_Directory_Obj
                  .Contains_Infrastructure (Stretch_Id),
                  "The Stretch directory does not "
                  & "contains the Stretch id");

      Ass.Assert (Added = TRUE,
                  "Added flag should have been set to true");
   end Test_Add;

   procedure Test_Contains_Infrastructure (T : in out TC.Test_Case'Class)
   is
      Stretch_Id : Infra_Id := 63;
      Stretch : access Stretch_Mock.Object'Class
        := Stretch_Mock.Create;
      Stretch_Ref : aliased Reactive.Infrastructure.Stretch.Reference
         := Reactive.Infrastructure.Stretch.Reference (Stretch);
      Added : Boolean := FALSE;
   begin
      Stretch.Set_Id (Stretch_Id);

      Ass.Assert (not Stretch_Directory_Obj
                  .Contains_Infrastructure (Stretch_Id),
                  "The Stretch directory already contains the Stretch");

      Stretch_Directory_Obj.Add
         (Infrastructure => Stretch_Ref,
          Added          => Added);

      Ass.Assert (Stretch_Directory_Obj
                  .Contains_Infrastructure (Stretch_Id),
                  "The Stretch directory does not "
                  & "contains the Stretch id");
   end Test_Contains_Infrastructure;

   procedure Test_Find_By_Id (T : in out TC.Test_Case'Class)
   is
      Stretch_Id : Infra_Id := 27;
      Stretch : access Stretch_Mock.Object'Class
        := Stretch_Mock.Create;
      Stretch_Ref : aliased Reactive.Infrastructure.Stretch.Reference
         := Reactive.Infrastructure.Stretch.Reference (Stretch);
      Found_Stretch : access Reactive.Infrastructure.Stretch.Object'Class;
      Added : Boolean := FALSE;
   begin
      Stretch.Set_Id (Stretch_Id);

      Ass.Assert (not Stretch_Directory_Obj
                  .Contains_Infrastructure (Stretch_Id),
                  "The Stretch directory already contains the Stretch");

      Stretch_Directory_Obj.Add
         (Infrastructure => Stretch_Ref,
          Added          => Added);

      Found_Stretch := Stretch_Directory_Obj.Find_By_Id (Stretch_Id);

      Ass.Assert (Found_Stretch = Stretch,
                  "The Stretch directory does not return "
                  & "the right Stretch");
   end Test_Find_By_Id;

   procedure Test_Safe_Find_By_Id (T : in out TC.Test_Case'Class)
   is
      Stretch_Id : Infra_Id := 27;
      Stretch : access Stretch_Mock.Object'Class
        := Stretch_Mock.Create;
      Stretch_Ref : aliased Reactive.Infrastructure.Stretch.Reference
         := Reactive.Infrastructure.Stretch.Reference (Stretch);
      Found_Stretch : access Reactive.Infrastructure.Stretch.Object'Class;
      Added : Boolean := FALSE;
      Found : Boolean := FALSE;
   begin
      Stretch.Set_Id (Stretch_Id);

      Ass.Assert (not Stretch_Directory_Obj
                  .Contains_Infrastructure (Stretch_Id),
                  "The Stretch directory already contains the Stretch");

      Stretch_Directory_Obj.Add
         (Infrastructure => Stretch_Ref,
          Added          => Added);

      Found_Stretch := Stretch_Directory_Obj.Safe_Find_By_Id (Stretch_Id,
         Found);

      Ass.Assert (Found,
                  "Find operation was not successful");

      Ass.Assert (Found_Stretch = Stretch,
                  "The Stretch directory does not return "
                  & "the right Stretch");
   end Test_Safe_Find_By_Id;

   -----------------------------------------------------
   --                  REGISTRATION
   -----------------------------------------------------
   procedure Register_Tests (T: in out Stretch_Directory_Test) is
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

   function Name(T: Stretch_Directory_Test) return AU.Message_String is
   begin
      return AU.Format ("Stretch_Directory");
   end Name;
end Reactive.Directory.Stretch_Directory.Tests;
