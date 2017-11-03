with AUnit.Assertions;
with Ada.Text_IO;

with Active.Agent;

with Reactive.District.Mock;
with Reactive.Infrastructure.Way.Roadway.Mock;

package body Reactive.Infrastructure.Way.Roadway.Utils.Tests is
   package Ass renames AUnit.Assertions;
   package Agent renames Active.Agent;
   package District_Mock renames Reactive.District.Mock;
   package Roadway_Mock renames Reactive.Infrastructure.Way.Roadway.Mock;

   -----------------------------------------------------
   --                   SCAFFOLDING
   -----------------------------------------------------
   Roadway_Utils : Reactive.Infrastructure.Way.Roadway.Utils.Reference;
   District : access District_Mock.Object;

   procedure Set_Up (T: in out Roadway_Utils_Test) is
   begin
      District := District_Mock.Create;

      Roadway_Utils
        := Reactive.Infrastructure.Way.Roadway.Utils.Get_Instance (District => District);
   end Set_Up;

   procedure Test_Lane_By_Direction_Finder (T: in out TC.Test_Case'Class)
   is
      Roadway_Mock_Ref : Roadway_Mock.Reference := Roadway_Mock.Create;
      Roadway : aliased Reactive.Infrastructure.Way.Roadway.Reference
        := Reactive.Infrastructure.Way.Roadway.Reference (Roadway_Mock_Ref);
      Roadway_Id : Infra_Id := 345;
      Found_Lane_Id : Infra_Id;
      Expected_Lane_Id : Infra_Id := 536;
      Added, Lane_Found : Boolean := FALSE;
   begin
      Roadway_Mock_Ref.Set_Id (Roadway_Id);
      District.Add_Roadway (Infrastructure => Roadway,
                                   Added          => Added);

      Roadway_Mock_Ref.Set_Return_Value_For_Add_Lane (TRUE);
      Roadway_Mock_Ref.Set_Return_Value_For_Find_Lane_By_Direction
        (Lane_Id => Expected_Lane_Id,
         Found   => TRUE);

      Roadway_Utils.Find_Lane_By_Direction
        (Roadway_Id       => Roadway_Id,
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
   procedure Register_Tests (T: in out Roadway_Utils_Test) is
      use TC.Registration;
   begin

      Register_Routine (Test => T,
                        Routine => Test_Lane_By_Direction_Finder'Access,
                        Name => "Test finder of lane by direction");

   end Register_Tests;

   function Name(T: Roadway_Utils_Test) return AU.Message_String is
   begin
      return AU.Format ("Roadway_Utils");
   end Name;
end Reactive.Infrastructure.Way.Roadway.Utils.Tests;
