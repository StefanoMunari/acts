with AUnit.Assertions;
with Active.Traffic_Light.Mock;

use Active.Traffic_Light;

package body Reactive.Traffic_Light_Registry.Tests is
   package Ass renames AUnit.Assertions;
   package Traffic_Light_Mock renames Active.Traffic_Light.Mock;

   use Agent;

   -----------------------------------------------------
   --                   SCAFFOLDING
   -----------------------------------------------------
   Traffic_Light_Registry_Ref : Reactive.Traffic_Light_Registry.Reference;

   procedure Set_Up_Case (T: in out Traffic_Light_Registry_Test) is
   begin
      Traffic_Light_Registry_Ref
        := Traffic_Light_Registry.Get_Instance;
   end Set_Up_Case;

   procedure Tear_Down (T: in out Traffic_Light_Registry_Test) is
   begin
      Traffic_Light_Registry_Ref.Population.Clear;
   end Tear_Down;

   -----------------------------------------------------
   --                   SCENARIOS
   -----------------------------------------------------

   procedure Test_Traffic_Light_Contains (T : in out TC.Test_Case'Class)
   is
      Traffic_Light_Id : Agent_Id := Create_Id_From_Natural (63);
      Traffic_Light    : aliased Traffic_Light_Mock.Object'Class
         := Traffic_Light_Mock.New_Mock.all;
      Added            : Boolean := FALSE;
   begin
      Traffic_Light.Set_Id (Traffic_Light_Id);

      Ass.Assert (not Traffic_Light_Registry_Ref.Population
                  .Contains_Traffic_Light (Traffic_Light_Id),
                  "The registry already contains the Traffic_Light");

      Traffic_Light_Registry_Ref.Population.Add_Traffic_Light
                 (Traffic_Light => Traffic_Light,
                  Added     => Added);

      Ass.Assert (
         Traffic_Light_Registry_Ref
            .Contains_Traffic_Light (Traffic_Light_Id),
         "The Traffic_Light registry does not contains the Traffic_Light id");
   end Test_Traffic_Light_Contains;

   procedure Test_Traffic_Light_Find (T : in out TC.Test_Case'Class)
   is
      Traffic_Light_Id    : Agent_Id := Create_Id_From_Natural (64);
      Traffic_Light       : aliased Traffic_Light_Mock.Object'Class
         := Traffic_Light_Mock.New_Mock.all;
      Found_Traffic_Light : access Active.Traffic_Light.Object'Class;
      Added               : Boolean := FALSE;
   begin
      Traffic_Light.Set_Id (Traffic_Light_Id);

      Ass.Assert (
         not Traffic_Light_Registry_Ref.Population
            .Contains_Traffic_Light (Traffic_Light_Id),
         "The Traffic_Light registry already contains the Traffic_Light");

      Traffic_Light_Registry_Ref.Population.Add_Traffic_Light (
         Traffic_Light => Traffic_Light,
         Added         => Added);

      Found_Traffic_Light := Traffic_Light_Registry_Ref
         .Find_Traffic_Light_By_Id (Traffic_Light_Id);

      Ass.Assert (
         Found_Traffic_Light = Traffic_Light'Access,
         "The Traffic_Light registry does not contains the Traffic_Light");
   end Test_Traffic_Light_Find;

   procedure Test_Traffic_Light_Add (T : in out TC.Test_Case'Class)
   is
      Traffic_Light_Id    : Agent_Id := Create_Id_From_Natural (65);
      Traffic_Light       : aliased Traffic_Light_Mock.Object'Class
         := Traffic_Light_Mock.New_Mock.all;
      Found_Traffic_Light : access Active.Traffic_Light.Object'Class;
      Added               : Boolean := FALSE;
   begin
      Traffic_Light.Set_Id (Traffic_Light_Id);

      Ass.Assert (not Traffic_Light_Registry_Ref.Population
                  .Contains_Traffic_Light (Traffic_Light_Id),
                  "The registry already contains the Traffic_Light");

      Traffic_Light_Registry_Ref.Add_Traffic_Light (
         Traffic_Light => Traffic_Light,
         Added         => Added);

      Found_Traffic_Light := Traffic_Light_Registry_Ref
         .Find_Traffic_Light_By_Id (Traffic_Light_Id);

      Ass.Assert (
         Found_Traffic_Light = Traffic_Light'Access,
         "The Traffic_Light registry does not contains the Traffic_Light");
   end Test_Traffic_Light_Add;

   procedure Test_Traffic_Light_Remove (T : in out TC.Test_Case'Class)
   is
      Traffic_Light_Id : Agent_Id := Create_Id_From_Natural (66);
      Traffic_Light : aliased Traffic_Light_Mock.Object'Class
        := Traffic_Light_Mock.New_Mock.all;
      Added, Removed : Boolean := FALSE;
   begin
      Traffic_Light.Set_Id (Traffic_Light_Id);

      Ass.Assert (
         not Traffic_Light_Registry_Ref.Population
               .Contains_Traffic_Light (Traffic_Light_Id),
         "The Traffic_Light registry already contains the Traffic_Light");

      Traffic_Light_Registry_Ref.Add_Traffic_Light (
         Traffic_Light => Traffic_Light,
         Added     => Added);

      Ass.Assert (Traffic_Light_Registry_Ref.Population
                  .Contains_Traffic_Light (Traffic_Light_Id),
                  "The Traffic_Light registry does not "
                  & "contains the Traffic_Light id");

      Traffic_Light_Registry_Ref.Remove_Traffic_Light (
         Traffic_Light_Id => Traffic_Light_Id,
         Removed      => Removed);

      Ass.Assert (not Traffic_Light_Registry_Ref.Population
                  .Contains_Traffic_Light (Traffic_Light_Id),
                  "The Traffic_Light registry does no more"
                  & "contains the Traffic_Light id");
   end Test_Traffic_Light_Remove;

   procedure Test_Remove_All_Traffic_Lights (T : in out TC.Test_Case'Class)
   is
      Traffic_Light1_Id : Agent_Id := Create_Id_From_Natural (67);
      Traffic_Light2_Id : Agent_Id := Create_Id_From_Natural (68);
      Traffic_Light3_Id : Agent_Id := Create_Id_From_Natural (69);
      Traffic_Light1, Traffic_Light2, Traffic_Light3 :
         aliased Traffic_Light_Mock.Object'Class
            := Traffic_Light_Mock.New_Mock.all;
      Added, Removed : Boolean := FALSE;
   begin
      Traffic_Light1.Set_Id (Traffic_Light1_Id);
      Traffic_Light2.Set_Id (Traffic_Light2_Id);
      Traffic_Light3.Set_Id (Traffic_Light3_Id);

      Ass.Assert (not Traffic_Light_Registry_Ref.Population
                  .Contains_Traffic_Light (Traffic_Light1_Id),
                  "The registry already contains the Traffic_Light1");

      Ass.Assert (not Traffic_Light_Registry_Ref.Population
                  .Contains_Traffic_Light (Traffic_Light2_Id),
                  "The registry already contains the Traffic_Light2");

      Ass.Assert (not Traffic_Light_Registry_Ref.Population
                  .Contains_Traffic_Light (Traffic_Light3_Id),
                  "The registry already contains the Traffic_Light3");

      Traffic_Light_Registry_Ref.Population.Add_Traffic_Light (
         Traffic_Light => Traffic_Light1,
         Added         => Added);

      Traffic_Light_Registry_Ref.Population.Add_Traffic_Light (
         Traffic_Light => Traffic_Light2,
         Added         => Added);

      Traffic_Light_Registry_Ref.Population.Add_Traffic_Light (
         Traffic_Light => Traffic_Light3,
         Added         => Added);

      Ass.Assert (
         Traffic_Light_Registry_Ref.Population
            .Contains_Traffic_Light (Traffic_Light1_Id),
         "The Traffic_Light registry does not contains the Traffic_Light1 id");

      Ass.Assert (
         Traffic_Light_Registry_Ref.Population
            .Contains_Traffic_Light (Traffic_Light2_Id),
         "The Traffic_Light registry does not contains the Traffic_Light2 id");

      Ass.Assert (
         Traffic_Light_Registry_Ref.Population
            .Contains_Traffic_Light (Traffic_Light3_Id),
         "The Traffic_Light registry does not contains the Traffic_Light3 id");

      Traffic_Light_Registry_Ref.Population.Clear;

      Ass.Assert (
         not Traffic_Light_Registry_Ref.Population
            .Contains_Traffic_Light (Traffic_Light1_Id),
         "The Traffic_Light registry contains the Traffic_Light1 id");

      Ass.Assert (
         not Traffic_Light_Registry_Ref.Population
            .Contains_Traffic_Light (Traffic_Light2_Id),
         "The Traffic_Light registry contains the Traffic_Light2 id");

      Ass.Assert (
         not Traffic_Light_Registry_Ref.Population
            .Contains_Traffic_Light (Traffic_Light3_Id),
         "The Traffic_Light registry contains the Traffic_Light3 id");
   end Test_Remove_All_Traffic_Lights;

   -----------------------------------------------------
   --                  REGISTRATION
   -----------------------------------------------------
   procedure Register_Tests (T: in out Traffic_Light_Registry_Test) is
      use TC.Registration;
   begin
      Register_Routine (Test => T,
                        Routine => Test_Traffic_Light_Contains'Access,
                        Name => "Test Traffic_Light contains");

      Register_Routine (Test => T,
                        Routine => Test_Traffic_Light_Find'Access,
                        Name => "Test Traffic_Light find");

      Register_Routine (Test => T,
                        Routine => Test_Traffic_Light_Add'Access,
                        Name => "Test Traffic_Light add");

      Register_Routine (Test => T,
                        Routine => Test_Traffic_Light_Remove'Access,
                        Name => "Test Traffic_Light remove");

      Register_Routine (Test => T,
                        Routine => Test_Remove_All_Traffic_Lights'Access,
                        Name => "Test remove all Traffic_Lights");
   end Register_Tests;

   function Name(T: Traffic_Light_Registry_Test) return AU.Message_String is
   begin
      return AU.Format ("Traffic_Light_Registry");
   end Name;
end Reactive.Traffic_Light_Registry.Tests;
