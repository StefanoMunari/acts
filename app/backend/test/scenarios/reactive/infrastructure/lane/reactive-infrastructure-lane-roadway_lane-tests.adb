with AUnit.Assertions;
with Ada.Text_IO;

with Active.Traveller.Utils.Mock;

with Reactive.Infrastructure.Lane.Roadway_Lane;
with Reactive.Infrastructure.Stretch.Utils.Mock;
with Reactive.Infrastructure.Way.Utils.Mock;

package body Reactive.Infrastructure.Lane.Roadway_Lane.Tests is
   package Ass renames AUnit.Assertions;
   package Traveller_Utils_Mock renames Active.Traveller.Utils.Mock;
   package Way_Utils renames Reactive.Infrastructure.Way.Utils.Mock;
   package Stretch_Utils renames Reactive.Infrastructure.Stretch.Utils.Mock;

   -----------------------------------------------------
   --                   SCAFFOLDING
   -----------------------------------------------------

   procedure Set_Up (T: in out Roadway_Lane_Test) is
      Street_Id       : Infra_Id           := 1;
      Roadway_Lane_Id : Infra_Id           := 2;
      Bus_Id          : Natural            := 10;
      Lane_Direction  : Direction.Straight := Direction.NORTH_SOUTH;
   begin
      T.Way_Utils := Way_Utils.Create;
      T.Stretch_Utils := Stretch_Utils.Create;
      T.Traveller_Utils := Traveller_Utils_Mock.Create;

      T.Lane
         := Lane.Reference (
            Roadway_Lane.Create (
               Id               => Roadway_Lane_Id,
               Direction        => Lane_Direction,
               Stretch_Utils    => T.Stretch_Utils,
               Way_Utils        => T.Way_Utils,
               Traveller_Utils  => T.Traveller_Utils));

      Lane.Tests.Set_Up (Lane.Tests.Lane_Test (T));
   end Set_Up;

   -----------------------------------------------------
   --                  REGISTRATION
   -----------------------------------------------------
   procedure Register_Tests (T: in out Roadway_Lane_Test) is
   begin
      null;
      --Lane.Tests.Register_Tests (Lane.Tests.Lane_Test (T));
   end Register_Tests;

   function Name(T: Roadway_Lane_Test) return AU.Message_String is
   begin
      return AU.Format ("Roadway_Lane");
   end Name;
end Reactive.Infrastructure.Lane.Roadway_Lane.Tests;
