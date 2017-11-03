with AUnit.Assertions;
with Reactive.Infrastructure.Intersection.Mock;

package body Reactive.Directory.Intersection_Directory.Tests is
   package Ass renames AUnit.Assertions;
   package Intersection_Mock renames  Reactive.Infrastructure.Intersection.Mock;

   -----------------------------------------------------
   --                   SCAFFOLDING
   -----------------------------------------------------
   Intersection_Directory_Obj : Reactive.Directory.Intersection_Directory.Directory;

   procedure Set_Up (T: in out Intersection_Directory_Test) is
   begin
      null;
   end Set_Up;

   procedure Tear_Down (T: in out Intersection_Directory_Test) is
   begin
      Intersection_Directory_Obj.Clear;
   end Tear_Down;

   -----------------------------------------------------
   --                   SCENARIOS
   -----------------------------------------------------

   procedure Test_Add (T : in out TC.Test_Case'Class)
   is
      Intersection_Id : Infra_Id := 42;
      Intersection : aliased Intersection_Mock.Object'Class
        := Intersection_Mock.Create.all;
      Added : Boolean := FALSE;
   begin
      Intersection.Set_Id (Intersection_Id);

      Ass.Assert (not Intersection_Directory_Obj
                  .Contains_Infrastructure (Intersection_Id),
                  "The Intersection directory already contains the Intersection");

      Intersection_Directory_Obj.Add
         (Infrastructure => Intersection,
          Added     => Added);

      Ass.Assert (Intersection_Directory_Obj
                  .Contains_Infrastructure (Intersection_Id),
                  "The Intersection directory does not "
                  & "contains the Intersection id");

      Ass.Assert (Added = TRUE,
                  "Added flag should have been set to true");
   end Test_Add;

   procedure Test_Contains_Infrastructure (T : in out TC.Test_Case'Class)
   is
      Intersection_Id : Infra_Id := 63;
      Intersection : aliased Intersection_Mock.Object'Class
        := Intersection_Mock.Create.all;
      Added : Boolean := FALSE;
   begin
      Intersection.Set_Id (Intersection_Id);

      Ass.Assert (not Intersection_Directory_Obj
                  .Contains_Infrastructure (Intersection_Id),
                  "The Intersection directory already contains the Intersection");

      Intersection_Directory_Obj.Add
         (Infrastructure => Intersection,
          Added     => Added);

      Ass.Assert (Intersection_Directory_Obj
                  .Contains_Infrastructure (Intersection_Id),
                  "The Intersection directory does not "
                  & "contains the Intersection id");
   end Test_Contains_Infrastructure;

   procedure Test_Find_By_Id (T : in out TC.Test_Case'Class)
   is
      Intersection_Id : Infra_Id := 27;
      Intersection : aliased Intersection_Mock.Object'Class
        := Intersection_Mock.Create.all;
      Found_Intersection : access Reactive.Infrastructure.Intersection.Object'Class;
      Added : Boolean := FALSE;
   begin
      Intersection.Set_Id (Intersection_Id);

      Ass.Assert (not Intersection_Directory_Obj
                  .Contains_Infrastructure (Intersection_Id),
                  "The Intersection directory already contains the Intersection");

      Intersection_Directory_Obj.Add
         (Infrastructure => Intersection,
          Added     => Added);

      Found_Intersection := Intersection_Directory_Obj.Find_By_Id (Intersection_Id);

      Ass.Assert (Found_Intersection = Intersection'Access,
                  "The Intersection directory does not return "
                  & "the right Intersection");
   end Test_Find_By_Id;

   procedure Test_Safe_Find_By_Id (T : in out TC.Test_Case'Class)
   is
      Intersection_Id : Infra_Id := 27;
      Intersection : aliased Intersection_Mock.Object'Class
        := Intersection_Mock.Create.all;
      Found_Intersection : access Reactive.Infrastructure.Intersection.Object'Class;
      Added : Boolean := FALSE;
      Found : Boolean := FALSE;
   begin
      Intersection.Set_Id (Intersection_Id);

      Ass.Assert (not Intersection_Directory_Obj
                  .Contains_Infrastructure (Intersection_Id),
                  "The Intersection directory already contains the Intersection");

      Intersection_Directory_Obj.Add
         (Infrastructure => Intersection,
          Added     => Added);

      Found_Intersection := Intersection_Directory_Obj.Safe_Find_By_Id (Intersection_Id,
         Found);

      Ass.Assert (Found,
                  "Find operation was not successful");

      Ass.Assert (Found_Intersection = Intersection'Access,
                  "The Intersection directory does not return "
                  & "the right Intersection");
   end Test_Safe_Find_By_Id;

   -----------------------------------------------------
   --                  REGISTRATION 
   -----------------------------------------------------
   procedure Register_Tests (T: in out Intersection_Directory_Test) is
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

   function Name(T: Intersection_Directory_Test) return AU.Message_String is
   begin
      return AU.Format ("Intersection_Directory");
   end Name;
end Reactive.Directory.Intersection_Directory.Tests;
