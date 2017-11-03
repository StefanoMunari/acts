with AUnit.Assertions;

with Reactive.Infrastructure.Street.Mock;

with Shared.Shared_References_Street;

package body Reactive.Directory.Street_Directory.Tests is
   package Ass           renames AUnit.Assertions;
   package Street_Mock   renames  Reactive.Infrastructure.Street.Mock;
-- shared
   package SR_Street_Pkg renames Shared.Shared_References_Street;

   -----------------------------------------------------
   --                   SCAFFOLDING
   -----------------------------------------------------
   Street_Directory_Obj : Reactive.Directory.Street_Directory.Directory;

   procedure Set_Up (T: in out Street_Directory_Test) is
   begin
      null;
   end Set_Up;

   procedure Tear_Down (T: in out Street_Directory_Test) is
   begin
      Street_Directory_Obj.Clear;
   end Tear_Down;

   -----------------------------------------------------
   --                   SCENARIOS
   -----------------------------------------------------

   procedure Test_Add (T : in out TC.Test_Case'Class)
   is
      Street_Id  : Infra_Id := 42;
      Street     : access Street_Mock.Object'Class
        := Street_Mock.Create;
      Street_Ref : aliased Reactive.Infrastructure.Street.Reference
         := Reactive.Infrastructure.Street.Reference (Street);
      SR_Street  : SR_Street_Pkg.Shared_Reference;
      Added      : Boolean := FALSE;
   begin
      Street.Set_Id (Street_Id);
      SR_Street.Init (SR_Street_Pkg.T_Reference (Street_Ref));

      Ass.Assert (not Street_Directory_Obj.Contains_Infrastructure (Street_Id),
                  "The Street directory already contains the Street");

      Street_Directory_Obj.Add (
         SR_Street => SR_Street,
         Added     => Added);

      Ass.Assert (Street_Directory_Obj
                  .Contains_Infrastructure (Street_Id),
                  "The Street directory does not "
                  & "contains the Street id");

      Ass.Assert (Added = TRUE,
                  "Added flag should have been set to true");
   end Test_Add;

   procedure Test_Contains_Infrastructure (T : in out TC.Test_Case'Class)
   is
      Street_Id : Infra_Id := 63;
      Street : access Street_Mock.Object'Class
        := Street_Mock.Create;
      Street_Ref : aliased Reactive.Infrastructure.Street.Reference
         := Reactive.Infrastructure.Street.Reference (Street);
      SR_Street  : SR_Street_Pkg.Shared_Reference;
      Added : Boolean := FALSE;
   begin
      Street.Set_Id (Street_Id);
      SR_Street.Init (SR_Street_Pkg.T_Reference (Street_Ref));

      Ass.Assert (not Street_Directory_Obj.Contains_Infrastructure (Street_Id),
                  "The Street directory already contains the Street");

      Street_Directory_Obj.Add (
         SR_Street => SR_Street,
         Added     => Added);

      Ass.Assert (
         Street_Directory_Obj.Contains_Infrastructure (Street_Id),
         "The Street directory does not contain the Street id");
   end Test_Contains_Infrastructure;

   procedure Test_Find_By_Id (T : in out TC.Test_Case'Class)
   is
      Street_Id : Infra_Id := 27;
      Street : access Street_Mock.Object'Class
        := Street_Mock.Create;
      Street_Ref : aliased Reactive.Infrastructure.Street.Reference
         := Reactive.Infrastructure.Street.Reference (Street);
      SR_Street  : SR_Street_Pkg.Shared_Reference;
      Found_Street : access Reactive.Infrastructure.Street.Object'Class;
      Added : Boolean := FALSE;
   begin
      Street.Set_Id (Street_Id);
      SR_Street.Init (SR_Street_Pkg.T_Reference (Street_Ref));

      Ass.Assert (
         not Street_Directory_Obj.Contains_Infrastructure (Street_Id),
         "The Street directory already contains the Street");

      Street_Directory_Obj.Add (
         SR_Street => SR_Street,
         Added     => Added);

      Found_Street := Street_Directory_Obj.Find_By_Id (Street_Id);

      Ass.Assert (Found_Street = Street_Ref,
                  "The Street directory does not return "
                  & "the right Street");
   end Test_Find_By_Id;

   procedure Test_Safe_Find_By_Id (T : in out TC.Test_Case'Class)
   is
      Street_Id : Infra_Id := 27;
      Street : access Street_Mock.Object'Class
        := Street_Mock.Create;
      Street_Ref : aliased Reactive.Infrastructure.Street.Reference
         := Reactive.Infrastructure.Street.Reference (Street);
      SR_Street  : SR_Street_Pkg.Shared_Reference;
      Found_Street : access Reactive.Infrastructure.Street.Object'Class;
      Added : Boolean := FALSE;
      Found : Boolean := FALSE;
   begin
      Street.Set_Id (Street_Id);
      SR_Street.Init (SR_Street_Pkg.T_Reference (Street_Ref));

      Ass.Assert (
         not Street_Directory_Obj.Contains_Infrastructure (Street_Id),
         "The Street directory already contains the Street");

      Street_Directory_Obj.Add (
         SR_Street => SR_Street,
         Added     => Added);

      Found_Street :=
         Street_Directory_Obj.Safe_Find_By_Id (Street_Id, Found);

      Ass.Assert (Found,
                  "Find operation was not successful");

      Ass.Assert (Found_Street = Street_Ref,
                  "The Street directory does not return "
                  & "the right Street");
   end Test_Safe_Find_By_Id;

   -----------------------------------------------------
   --                  REGISTRATION
   -----------------------------------------------------
   procedure Register_Tests (T: in out Street_Directory_Test) is
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

   function Name(T: Street_Directory_Test) return AU.Message_String is
   begin
      return AU.Format ("Street_Directory");
   end Name;
end Reactive.Directory.Street_Directory.Tests;
