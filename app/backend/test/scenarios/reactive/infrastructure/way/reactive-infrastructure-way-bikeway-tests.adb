with AUnit.Assertions;
with Ada.Text_IO;

with Reactive.Infrastructure.Way.Bikeway;
with Reactive.Infrastructure.Lane.Utils.Mock;
with Reactive.Infrastructure.Street.Utils.Mock;
with Active.Traveller.Utils.Mock;

package body Reactive.Infrastructure.Way.Bikeway.Tests is
   package Ass renames AUnit.Assertions;
   package Traveller_Utils renames Active.Traveller.Utils.Mock;
   package Lane_Utils renames Reactive.Infrastructure.Lane.Utils.Mock;
   package Street_Utils renames Reactive.Infrastructure.Street.Utils.Mock;

   -----------------------------------------------------
   --                   SCAFFOLDING
   -----------------------------------------------------
   Street_Id : Infra_Id;
   Bikeway_Id : Infra_Id;
   Bus_Id : Natural;

   procedure Set_Up (T: in out Bikeway_Test) is
   begin
      Street_Id := 1;
      Bikeway_Id := 2;
      Bus_Id := 10;
      T.Traveller_Utils := Traveller_Utils.Create;
      T.Lane_Utils := Lane_Utils.Create;
      T.Street_Utils := Street_Utils.Create;

      T.Way
        := Way.Reference
          (Bikeway.Create (Id              => Bikeway_Id,
                           Traveller_Utils => T.Traveller_Utils,
                           Lane_Utils      => T.Lane_Utils,
                           Street_Utils    => T.Street_Utils));

      Way.Tests.Set_Up (Way.Tests.Way_Test (T));
   end Set_Up;

   -----------------------------------------------------
   --                  REGISTRATION
   -----------------------------------------------------
   procedure Register_Tests (T: in out Bikeway_Test) is
   begin
      Way.Tests.Register_Tests (Way.Tests.Way_Test (T));
   end Register_Tests;

   function Name(T: Bikeway_Test) return AU.Message_String is
   begin
      return AU.Format ("Bikeway");
   end Name;
end Reactive.Infrastructure.Way.Bikeway.Tests;
