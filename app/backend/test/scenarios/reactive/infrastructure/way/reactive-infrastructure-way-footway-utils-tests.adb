with AUnit.Assertions;
with Ada.Text_IO;

with Active.Agent;

with Reactive.District.Mock;
with Reactive.Infrastructure.Way.Footway.Mock;

package body Reactive.Infrastructure.Way.Footway.Utils.Tests is
   package Ass renames AUnit.Assertions;
   package Agent renames Active.Agent;
   package District_Mock renames Reactive.District.Mock;
   package Footway_Mock renames Reactive.Infrastructure.Way.Footway.Mock;

   -----------------------------------------------------
   --                   SCAFFOLDING
   -----------------------------------------------------
   Footway_Utils : Reactive.Infrastructure.Way.Footway.Utils.Reference;
   District : access District_Mock.Object;

   procedure Set_Up (T: in out Footway_Utils_Test) is
   begin
      District := District_Mock.Create;

      Footway_Utils
         := Reactive.Infrastructure.Way.Footway.Utils.Get_Instance
            (District => District);
   end Set_Up;

   procedure Test_Lane_By_Direction_Finder (T: in out TC.Test_Case'Class)
   is
      Footway_Mock_Ref : Footway_Mock.Reference := Footway_Mock.Create;
      Footway : aliased Reactive.Infrastructure.Way.Footway.Reference
        := Reactive.Infrastructure.Way.Footway.Reference (Footway_Mock_Ref);
      Footway_Id : Infra_Id := 345;
      Found_Lane_Id : Infra_Id;
      Expected_Lane_Id : Infra_Id := 536;
      Added, Lane_Found : Boolean := FALSE;
   begin
      Footway_Mock_Ref.Set_Id (Footway_Id);
      District.Add_Footway (Infrastructure => Footway,
                                   Added          => Added);

      Footway_Mock_Ref.Set_Return_Value_For_Add_Lane (TRUE);
      Footway_Mock_Ref.Set_Return_Value_For_Find_Lane_By_Direction
         (Lane_Id => Expected_Lane_Id, Found => TRUE);

      Footway_Utils.Find_Lane_By_Direction
        (Footway_Id       => Footway_Id,
         Travel_Direction => Direction.NORTH_SOUTH,
         Lane_Id          => Found_Lane_Id,
         Found            => Lane_Found);

      Ass.Assert (Lane_Found,
                  "The lane is not found");

      Ass.Assert (Found_Lane_Id = Expected_Lane_Id,
                  "The lane is not found");
   end Test_Lane_By_Direction_Finder;

   -----------------------------------------------------
   --                  REGISTRATION
   -----------------------------------------------------
   procedure Register_Tests (T: in out Footway_Utils_Test) is
      use TC.Registration;
   begin

      Register_Routine (Test => T,
                        Routine => Test_Lane_By_Direction_Finder'Access,
                        Name => "Test finder of lane by direction");

   end Register_Tests;

   function Name(T: Footway_Utils_Test) return AU.Message_String is
   begin
      return AU.Format ("Footway_Utils");
   end Name;
end Reactive.Infrastructure.Way.Footway.Utils.Tests;
