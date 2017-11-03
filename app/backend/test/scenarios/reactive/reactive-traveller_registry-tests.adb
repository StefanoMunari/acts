with AUnit.Assertions;

with Active.Agent;
with Active.Traveller;
with Active.Traveller.Mock;

package body Reactive.Traveller_Registry.Tests is
   package Ass renames AUnit.Assertions;
   package Agent renames Active.Agent;
   package Traveller_Pkg renames Active.Traveller;
   package Traveller_Mock renames Active.Traveller.Mock;

   -----------------------------------------------------
   --                   SCAFFOLDING
   -----------------------------------------------------
   Traveller_Registry_Ref : Reactive.Traveller_Registry.Reference;

   procedure Set_Up_Case (T: in out Traveller_Registry_Test) is
   begin
      Traveller_Registry_Ref
        := Traveller_Registry.Get_Instance;
   end Set_Up_Case;

   procedure Tear_Down (T: in out Traveller_Registry_Test) is
   begin
      Traveller_Registry_Ref.Population.Clear;
   end Tear_Down;

   -----------------------------------------------------
   --                   SCENARIOS
   -----------------------------------------------------

   procedure Test_Traveller_Contains (T : in out TC.Test_Case'Class)
   is
      Traveller_Id : Agent.Agent_Id := Agent.Create_Id_From_Natural (604);
      Traveller_Obj : aliased Traveller_Mock.Object'Class
        := Traveller_Mock.Create.all;
      Traveller_Ref : Traveller_Pkg.Reference :=
         Traveller_Obj'Unchecked_Access;
      Added : Boolean := FALSE;
   begin
      Traveller_Obj.Set_Id (Traveller_Id);

      Ass.Assert (
         not Traveller_Registry_Ref.Population.Contains_Traveller (
            Traveller_Id),
         "The traveller registry already contains the traveller");

      Traveller_Registry_Ref.Population.Add_Traveller (
         Traveller => Traveller_Ref,
         Added     => Added);

      Ass.Assert (Traveller_Registry_Ref.Contains_Traveller (Traveller_Id),
                  "The traveller registry does not contains the traveller id");
   end Test_Traveller_Contains;

   procedure Test_Traveller_Find (T : in out TC.Test_Case'Class)
   is
      Traveller_Id : Agent.Agent_Id := Agent.Create_Id_From_Natural (605);
      Traveller_Obj : aliased Traveller_Mock.Object'Class
        := Traveller_Mock.Create.all;
      Traveller_Ref : Traveller_Pkg.Reference :=
         Traveller_Obj'Unchecked_Access;
      Found_Traveller : access Active.Traveller.Object'Class;
      Added : Boolean := FALSE;
   begin
      Traveller_Obj.Set_Id (Traveller_Id);

      Ass.Assert (
         not Traveller_Registry_Ref.Population.Contains_Traveller (
            Traveller_Id),
         "The traveller registry already contains the traveller");

      Traveller_Registry_Ref.Population.Add_Traveller (
         Traveller => Traveller_Ref,
         Added     => Added);

      Found_Traveller := Traveller_Registry_Ref.Find_Traveller_By_Id (
         Traveller_Id);

      Ass.Assert (
         Found_Traveller = Traveller_Obj'Access,
         "The traveller registry does not contains the traveller");
   end Test_Traveller_Find;

   procedure Test_Traveller_Add (T : in out TC.Test_Case'Class)
   is
      Traveller_Id : Agent.Agent_Id := Agent.Create_Id_From_Natural (606);
      Traveller_Obj : aliased Traveller_Mock.Object'Class
        := Traveller_Mock.Create.all;
      Traveller_Ref : Traveller_Pkg.Reference :=
         Traveller_Obj'Unchecked_Access;
      Found_Traveller : access Active.Traveller.Object'Class;
      Added : Boolean := FALSE;
   begin
      Traveller_Obj.Set_Id (Traveller_Id);

      Ass.Assert (
         not Traveller_Registry_Ref.Population.Contains_Traveller (
            Traveller_Id),
         "The traveller registry already contains the traveller");

      Traveller_Registry_Ref.Add_Traveller (Traveller => Traveller_Ref,
                                            Added     => Added);

      Found_Traveller := Traveller_Registry_Ref.Find_Traveller_By_Id (
         Traveller_Id);

      Ass.Assert (
         Found_Traveller = Traveller_Obj'Access,
         "The traveller registry does not contains the traveller");
   end Test_Traveller_Add;

   procedure Test_Traveller_Remove (T : in out TC.Test_Case'Class)
   is
      Traveller_Id : Agent.Agent_Id := Agent.Create_Id_From_Natural (607);
      Traveller_Obj : aliased Traveller_Mock.Object'Class
        := Traveller_Mock.Create.all;
      Traveller_Ref : Traveller_Pkg.Reference :=
         Traveller_Obj'Unchecked_Access;
      Added, Removed : Boolean := FALSE;
   begin
      Traveller_Obj.Set_Id (Traveller_Id);

      Ass.Assert (
         not Traveller_Registry_Ref.Population.Contains_Traveller (
            Traveller_Id),
         "The traveller registry already contains the traveller");

      Traveller_Registry_Ref.Add_Traveller (Traveller => Traveller_Ref,
                                            Added     => Added);

      Ass.Assert (
         Traveller_Registry_Ref.Population.Contains_Traveller (Traveller_Id),
         "The traveller registry does not contains the traveller id");

      Traveller_Registry_Ref.Remove_Traveller (Traveller_Id => Traveller_Id,
                                               Removed      => Removed);

      Ass.Assert (
         not Traveller_Registry_Ref.Population.Contains_Traveller (
            Traveller_Id),
         "The traveller registry does no more contains the traveller id");
   end Test_Traveller_Remove;

   procedure Test_Remove_All_Travellers (T : in out TC.Test_Case'Class)
   is
      Traveller1_Id  : Agent.Agent_Id := Agent.Create_Id_From_Natural (608);
      Traveller2_Id  : Agent.Agent_Id := Agent.Create_Id_From_Natural (609);
      Traveller3_Id  : Agent.Agent_Id := Agent.Create_Id_From_Natural (610);
      Traveller1_Obj : aliased Traveller_Mock.Object'Class
        := Traveller_Mock.Create.all;
      Traveller1_Ref : Traveller_Pkg.Reference :=
         Traveller1_Obj'Unchecked_Access;
      Traveller2_Obj : aliased Traveller_Mock.Object'Class
        := Traveller_Mock.Create.all;
      Traveller2_Ref : Traveller_Pkg.Reference :=
         Traveller2_Obj'Unchecked_Access;
      Traveller3_Obj : aliased Traveller_Mock.Object'Class
        := Traveller_Mock.Create.all;
      Traveller3_Ref : Traveller_Pkg.Reference :=
         Traveller3_Obj'Unchecked_Access;
      Added, Removed : Boolean := FALSE;
   begin
      Traveller1_Obj.Set_Id (Traveller1_Id);
      Traveller2_Obj.Set_Id (Traveller2_Id);
      Traveller3_Obj.Set_Id (Traveller3_Id);

      Ass.Assert (
         not Traveller_Registry_Ref.Population.Contains_Traveller (
            Traveller1_Id),
         "The traveller registry already contains the traveller1");

      Ass.Assert (
         not Traveller_Registry_Ref.Population.Contains_Traveller (
            Traveller2_Id),
         "The traveller registry already contains the traveller2");

      Ass.Assert (
         not Traveller_Registry_Ref.Population.Contains_Traveller (
            Traveller3_Id),
         "The traveller registry already contains the traveller3");

      Traveller_Registry_Ref.Population.Add_Traveller (
         Traveller => Traveller1_Ref,
         Added     => Added);

      Traveller_Registry_Ref.Population.Add_Traveller (
         Traveller => Traveller2_Ref,
         Added     => Added);

      Traveller_Registry_Ref.Population.Add_Traveller (
         Traveller => Traveller3_Ref,
         Added     => Added);

      Ass.Assert (
         Traveller_Registry_Ref.Population.Contains_Traveller (Traveller1_Id),
         "The traveller registry does not contain the traveller1 id");

      Ass.Assert (
         Traveller_Registry_Ref.Population.Contains_Traveller (Traveller2_Id),
         "The traveller registry does not contain the traveller2 id");

      Ass.Assert (
         Traveller_Registry_Ref.Population.Contains_Traveller (Traveller3_Id),
         "The traveller registry does not contain the traveller3 id");

      Traveller_Registry_Ref.Population.Clear;

      Ass.Assert (
         not Traveller_Registry_Ref.Population.Contains_Traveller (
            Traveller1_Id),
         "The traveller registry contains the traveller1 id");

      Ass.Assert (
         not Traveller_Registry_Ref.Population.Contains_Traveller (
            Traveller2_Id),
         "The traveller registry contains the traveller2 id");

      Ass.Assert (
         not Traveller_Registry_Ref.Population.Contains_Traveller (
            Traveller3_Id),
         "The traveller registry contains the traveller3 id");
   end Test_Remove_All_Travellers;

   -----------------------------------------------------
   --                  REGISTRATION
   -----------------------------------------------------
   procedure Register_Tests (T: in out Traveller_Registry_Test) is
      use TC.Registration;
   begin
      Register_Routine (Test => T,
                        Routine => Test_Traveller_Contains'Access,
                        Name => "Test traveller contains");

      Register_Routine (Test => T,
                        Routine => Test_Traveller_Find'Access,
                        Name => "Test traveller find");

      Register_Routine (Test => T,
                        Routine => Test_Traveller_Add'Access,
                        Name => "Test traveller add");

      Register_Routine (Test => T,
                        Routine => Test_Traveller_Remove'Access,
                        Name => "Test traveller remove");

      Register_Routine (Test => T,
                        Routine => Test_Remove_All_Travellers'Access,
                        Name => "Test remove all travellers");
   end Register_Tests;

   function Name(T: Traveller_Registry_Test) return AU.Message_String is
   begin
      return AU.Format ("Traveller_Registry");
   end Name;
end Reactive.Traveller_Registry.Tests;
