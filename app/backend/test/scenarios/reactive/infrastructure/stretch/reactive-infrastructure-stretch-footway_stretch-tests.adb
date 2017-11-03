with AUnit.Assertions;
with Ada.Text_IO;

with Active.Traveller.Utils.Mock;

with Reactive.Infrastructure.Building.Host.Utils.Mock;
with Reactive.Infrastructure.Stretch.Footway_Stretch;
with Reactive.Infrastructure.Lane.Utils.Mock;

package body Reactive.Infrastructure.Stretch.Footway_Stretch.Tests is
   package Ass renames AUnit.Assertions;
   package Lane_Utils renames Reactive.Infrastructure.Lane.Utils.Mock;
   package Traveller_Utils renames Active.Traveller.Utils.Mock;
   package Host_Utils_Mock_Pkg
      renames Reactive.Infrastructure.Building.Host.Utils.Mock;

   -----------------------------------------------------
   --                   SCAFFOLDING
   -----------------------------------------------------
   Street_Id : Infra_Id;
   Footway_Stretch_Id : Infra_Id;
   Bus_Id : Natural;
   Stretch_Size : Natural;

   procedure Set_Up (T: in out Footway_Stretch_Test) is
   begin
      Street_Id := 1;
      Footway_Stretch_Id := 2;
      Bus_Id := 10;
      Stretch_Size := 4;
      T.Lane_Utils := Lane_Utils.Create;
      T.Traveller_Utils := Traveller_Utils.Create;
      T.Traveller_Utils.Set_Return_Value_For_Get_Size (1);
      T.Host_Utils := Host_Utils_Mock_Pkg.Create;

      T.Stretch
         := Stretch.Reference (
            Footway_Stretch.Create (
               Id              => Footway_Stretch_Id,
               Size            => Stretch_Size,
               Host_Utils      => T.Host_Utils,
               Lane_Utils      => T.Lane_Utils,
               Traveller_Utils => T.Traveller_Utils)
            );

      Stretch.Tests.Set_Up (Stretch.Tests.Stretch_Test (T));
   end Set_Up;

   -----------------------------------------------------
   --                  REGISTRATION
   -----------------------------------------------------
   procedure Register_Tests (T: in out Footway_Stretch_Test) is
   begin
      Stretch.Tests.Register_Tests (Stretch.Tests.Stretch_Test (T));
   end Register_Tests;

   function Name(T: Footway_Stretch_Test) return AU.Message_String is
   begin
      return AU.Format ("Footway_Stretch");
   end Name;
end Reactive.Infrastructure.Stretch.Footway_Stretch.Tests;
