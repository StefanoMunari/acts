with AUnit.Assertions;

package body Reactive.Infrastructure.Factory.Street_Factory.Roadway_Factory.Tests is
   package Ass renames AUnit.Assertions;

   -----------------------------------------------------
   --                   SCAFFOLDING
   -----------------------------------------------------
   Factory_Ref : Reactive.Infrastructure.Factory.Street_Factory.Roadway_Factory.Reference;

   procedure Set_Up (T : in out Roadway_Factory_Test) is
   begin
      Factory_Ref := new Roadway_Factory.Object;
   end Set_Up;

   -- Test Routines:
   procedure Test_Create_Roadway_Stretch (T: in out TC.Test_Case'Class) is
      Stretch_Id   : Infra_Id := 42;
      Stretch_Size : Natural  := 7;
      Stretch_Ref  : Stretch.Reference;
   begin
      Factory_Ref.Set_Stretch_Id (Stretch_Id);
      Factory_Ref.Set_Stretch_Size (Stretch_Size);
      Stretch_Ref := Factory_Ref.Create_Stretch;

      Ass.Assert (Stretch_Id = Stretch_Ref.Get_Id,
         "Id was not correctly set");
      Ass.Assert (Stretch_Size = Stretch_Ref.Get_Size,
         "Size was not correctly set");
   end Test_Create_Roadway_Stretch;

   procedure Test_Create_Roadway_Lane (T: in out TC.Test_Case'Class) is
      Lane_Id        : Infra_Id := 42;
      Lane_Direction : Shared.Direction.Straight := NORTH_SOUTH;
      Lane_Ref       : Lane.Reference;
   begin
      Factory_Ref.Set_Lane_Id (Lane_Id);
      Factory_Ref.Set_Lane_Direction (Lane_Direction);
      Lane_Ref := Factory_Ref.Create_Lane;

      Ass.Assert (Lane_Id = Lane_Ref.Get_Id,
         "Id was not correctly set");
      Ass.Assert (Lane_Direction = Lane_Ref.Get_Direction,
         "Direction was not correctly set");
   end Test_Create_Roadway_Lane;

   procedure Register_Tests (T : in out Roadway_Factory_Test) is
      use TC.Registration;
   begin
      Register_Routine (Test    => T,
                        Routine => Test_Create_Roadway_Stretch'Access,
                        Name    => "Test stretch creation");

      Register_Routine (Test    => T,
                        Routine => Test_Create_Roadway_Lane'Access,
                        Name    => "Test lane creation");
   end Register_Tests;

   function Name (T : in Roadway_Factory_Test)
      return AU.Message_String is
   begin
      return AU.Format ("Roadway_Factory");
   end Name;

end Reactive.Infrastructure.Factory.Street_Factory.Roadway_Factory.Tests;
