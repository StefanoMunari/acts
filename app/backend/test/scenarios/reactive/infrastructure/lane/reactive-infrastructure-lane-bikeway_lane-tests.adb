with AUnit.Assertions;
with Ada.Text_IO;

with Active.Traveller.Utils.Mock;

with Reactive.Infrastructure.Lane.Bikeway_Lane;
with Reactive.Infrastructure.Way.Utils.Mock;
with Reactive.Infrastructure.Stretch.Utils.Mock;

package body Reactive.Infrastructure.Lane.Bikeway_Lane.Tests is
   package Ass renames AUnit.Assertions;
   package Traveller_Utils renames Active.Traveller.Utils.Mock;
   package Way_Utils renames Reactive.Infrastructure.Way.Utils.Mock;
   package Stretch_Utils renames Reactive.Infrastructure.Stretch.Utils.Mock;

   -----------------------------------------------------
   --                   SCAFFOLDING
   -----------------------------------------------------
   Street_Id : Infra_Id;
   Bikeway_Lane_Id : Infra_Id;
   Bus_Id : Natural;
   Lane_Direction : Direction.Straight;

   procedure Set_Up (T: in out Bikeway_Lane_Test) is
   begin
      Street_Id := 1;
      Bikeway_Lane_Id := 2;
      Bus_Id := 10;
      Lane_Direction := Direction.NORTH_SOUTH;
      T.Way_Utils := Way_Utils.Create;
      T.Stretch_Utils := Stretch_Utils.Create;
      T.Traveller_Utils := Traveller_Utils.Create;

      T.Lane
        := Lane.Reference
          (Bikeway_Lane.Create (Id               => Bikeway_Lane_Id,
                                Direction        => Lane_Direction,
                                Stretch_Utils    => T.Stretch_Utils,
                                Way_Utils        => T.Way_Utils,
                                Traveller_Utils  => T.Traveller_Utils));

      Lane.Tests.Set_Up (Lane.Tests.Lane_Test (T));
   end Set_Up;

   -----------------------------------------------------
   --                  REGISTRATION
   -----------------------------------------------------
   procedure Register_Tests (T: in out Bikeway_Lane_Test) is
   begin
      Lane.Tests.Register_Tests (Lane.Tests.Lane_Test (T));
   end Register_Tests;

   function Name(T: Bikeway_Lane_Test) return AU.Message_String is
   begin
      return AU.Format ("Bikeway_Lane");
   end Name;
end Reactive.Infrastructure.Lane.Bikeway_Lane.Tests;
