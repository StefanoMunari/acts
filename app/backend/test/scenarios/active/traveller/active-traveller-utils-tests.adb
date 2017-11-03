with AUnit.Assertions;
with Ada.Text_IO;

with Active.Agent;
with Active.Traveller.Mock;

with Reactive.District.Mock;

package body Active.Traveller.Utils.Tests is
   package Ass renames AUnit.Assertions;
   package Agent renames Active.Agent;
   package District_Mock renames Reactive.District.Mock;
   package Traveller_Mock renames Active.Traveller.Mock;

   -----------------------------------------------------
   --                   SCAFFOLDING
   -----------------------------------------------------
   Traveller_Utils : Active.Traveller.Utils.Reference;
   District : access District_Mock.Object;

   procedure Set_Up (T: in out Traveller_Utils_Test) is
   begin
      District := District_Mock.Create;

      Traveller_Utils
        := Active.Traveller.Utils.Get_Instance (District => District);
   end Set_Up;

   -----------------------------------------------------
   --                  REGISTRATION
   -----------------------------------------------------
   procedure Register_Tests (T: in out Traveller_Utils_Test) is
      use TC.Registration;
   begin

      null;

   end Register_Tests;

   function Name(T: Traveller_Utils_Test) return AU.Message_String is
   begin
      return AU.Format ("Traveller_Utils");
   end Name;
end Active.Traveller.Utils.Tests;
