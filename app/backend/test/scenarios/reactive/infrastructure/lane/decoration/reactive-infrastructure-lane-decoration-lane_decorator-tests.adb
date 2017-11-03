with AUnit.Assertions;

package body Reactive.Infrastructure.Lane.Decoration.Lane_Decorator.Tests is

   package Ass renames AUnit.Assertions;

   -----------------------------------------------------
   --                   SCAFFOLDING
   -----------------------------------------------------
   procedure Set_Up (T : in out Lane_Decorator_Test) is
      Traveller_Added : Boolean := False;
   begin
      T.Lane_Ref  := Lane_Mock.Create;
      T.Decorator := new Concrete_Decorator;
      T.Decorator.Init (Lane.Reference (T.Lane_Ref));
   end Set_Up;

   -----------------------------------------------------
   --                   SCENARIOS
   -----------------------------------------------------

   procedure Test_Get_Lane_Ref (T : in out Lane_Decorator_Test)
   is
      Result : Lane.Reference;
   begin
      Result := T.Decorator.Get_Lane_Ref;
      Ass.Assert (Result = Lane.Reference (T.Lane_Ref),
                  "The decorator getter does not return the decorated lane");
   end Test_Get_Lane_Ref;

   -----------------------------------------------------
   --                 TEST WRAPPERS
   -----------------------------------------------------

   procedure Test_Get_Lane_Ref_Wrapper
      (T : in out Lane_Decorator_Test'Class)
   is
   begin
      Test_Get_Lane_Ref (T);
   end Test_Get_Lane_Ref_Wrapper;

    -----------------------------------------------------
   --                  REGISTRATION
   -----------------------------------------------------
   procedure Register_Tests (T: in out Lane_Decorator_Test) is
      package Register_Specific is
         new TC.Specific_Test_Case_Registration (Lane_Decorator_Test);
      use Register_Specific;
   begin

      Register_Wrapper
        (Test    => T,
         Routine => Test_Get_Lane_Ref_Wrapper'Access,
         Name    => "Tests a lane decoration returns the referred lane");

   end Register_Tests;

   function Name(T : Lane_Decorator_Test) return AU.Message_String is
   begin
      return AU.Format ("Lane_Decorator");
   end Name;

end Reactive.Infrastructure.Lane.Decoration.Lane_Decorator.Tests;
