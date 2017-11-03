with AUnit.Assertions;
with Ada.Text_IO;

with Active.Agent;

with Reactive.District.Mock;
with Reactive.Infrastructure.Way.Bikeway.Mock;

package body Reactive.Infrastructure.Way.Bikeway.Utils.Tests is
   package Ass renames AUnit.Assertions;
   package Agent renames Active.Agent;
   package District_Mock renames Reactive.District.Mock;
   package Bikeway_Mock renames Reactive.Infrastructure.Way.Bikeway.Mock;

   -----------------------------------------------------
   --                   SCAFFOLDING
   -----------------------------------------------------
   Bikeway_Utils : Reactive.Infrastructure.Way.Bikeway.Utils.Reference;
   District : access District_Mock.Object;

   procedure Set_Up (T: in out Bikeway_Utils_Test) is
   begin
      District := District_Mock.Create;

      Bikeway_Utils
         := Reactive.Infrastructure.Way.Bikeway.Utils.Get_Instance
            (District => District);
   end Set_Up;

   procedure Test_Lane_By_Direction_Finder (T: in out TC.Test_Case'Class)
   is
      Bikeway_Mock_Ref : Bikeway_Mock.Reference := Bikeway_Mock.Create;
      Bikeway : aliased Reactive.Infrastructure.Way.Bikeway.Reference
        := Reactive.Infrastructure.Way.Bikeway.Reference (Bikeway_Mock_Ref);
      Bikeway_Id : Infra_Id := 345;
      Found_Lane_Id : Infra_Id;
      Expected_Lane_Id : Infra_Id := 536;
      Added, Lane_Found : Boolean := FALSE;
   begin
      Bikeway_Mock_Ref.Set_Id (Bikeway_Id);
      District.Add_Bikeway (Infrastructure => Bikeway,
                                   Added          => Added);

      Bikeway_Mock_Ref.Set_Return_Value_For_Add_Lane (TRUE);
      Bikeway_Mock_Ref.Set_Return_Value_For_Find_Lane_By_Direction
        (Lane_Id => Expected_Lane_Id,
         Found   => TRUE);

      Bikeway_Utils.Find_Lane_By_Direction
        (Bikeway_Id       => Bikeway_Id,
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
   procedure Register_Tests (T: in out Bikeway_Utils_Test) is
      use TC.Registration;
   begin

      Register_Routine (Test => T,
                        Routine => Test_Lane_By_Direction_Finder'Access,
                        Name => "Test finder of lane by direction");

   end Register_Tests;

   function Name(T: Bikeway_Utils_Test) return AU.Message_String is
   begin
      return AU.Format ("Bikeway_Utils");
   end Name;
end Reactive.Infrastructure.Way.Bikeway.Utils.Tests;
