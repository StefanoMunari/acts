with AUnit.Assertions;

with Active.Traveller.Mock;

with Passive.Road_Sign.Mock;

package body Reactive.Infrastructure.Lane.Decoration.Lane_Sign_Decorator.Tests
is

   package Ass renames AUnit.Assertions;
   package Traveller_Mock renames Active.Traveller.Mock;
   package Sign_Mock renames Passive.Road_Sign.Mock;

   Traveller : aliased Traveller_Mock.Object'Class
      := Traveller_Mock.Create.all;
   Traveller_Id : Agent.Agent_Id := Agent.Create_Id_From_Natural (350);

   -----------------------------------------------------
   --                   SCAFFOLDING
   -----------------------------------------------------
   procedure Set_Up (T : in out Lane_Sign_Decorator_Test) is
      Traveller_Added : Boolean := False;
   begin
      T.Lane_Ref  := Lane_Mock.Create;
      T.Sign_Ref  := Sign_Mock.Create;
      T.Decorator := Create (Lane.Reference (T.Lane_Ref), T.Sign_Ref);
   end Set_Up;

   -----------------------------------------------------
   --                   SCENARIOS
   -----------------------------------------------------

   procedure Test_Enter (T : in out Lane_Sign_Decorator_Test)
   is
      Sign_Mock_Ref : Sign_Mock.Reference := Sign_Mock.Reference (T.Sign_Ref);
   begin
      T.Decorator.Enter (Traveller_Id);
      Ass.Assert (Sign_Mock_Ref.Get_Apply_For (Traveller_Id),
                  "The road signal was not applied to the traveller");
   end Test_Enter;

   -----------------------------------------------------
   --                 TEST WRAPPERS
   -----------------------------------------------------

   procedure Test_Enter_Wrapper (
      T : in out Lane_Sign_Decorator_Test'Class)
   is
   begin
      Test_Enter (T);
   end Test_Enter_Wrapper;

    -----------------------------------------------------
   --                  REGISTRATION
   -----------------------------------------------------
   procedure Register_Tests (T: in out Lane_Sign_Decorator_Test) is
      package Register_Specific is
         new TC.Specific_Test_Case_Registration (Lane_Sign_Decorator_Test);
      use Register_Specific;
   begin

      Register_Wrapper (
         Test    => T,
         Routine => Test_Enter_Wrapper'Access,
         Name    => "Tests a road signal is applied when entering a decorated"
                    & " lane");

   end Register_Tests;

   function Name(T : Lane_Sign_Decorator_Test) return AU.Message_String is
   begin
      return AU.Format ("Lane_Sign_Decorator");
   end Name;

end Reactive.Infrastructure.Lane.Decoration.Lane_Sign_Decorator.Tests;
