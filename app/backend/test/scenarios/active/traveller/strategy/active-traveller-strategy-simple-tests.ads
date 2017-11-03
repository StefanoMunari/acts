with AUnit;
with AUnit.Test_Cases;

with Active.Traveller.Utils.Mock;

package Active.Traveller.Strategy.Simple.Tests is
   package AU renames AUnit;
   package TC renames AUnit.Test_Cases;
   package Traveller_Utils_Mock_Pkg renames Active.Traveller.Utils.Mock;

   type Simple_Test is new TC.Test_Case with record
      Simple_Strategy : Strategy.Simple.Reference;
      Traveller_Utils : Traveller_Utils_Mock_Pkg.Reference;
   end record;

   overriding procedure Set_Up (T: in out Simple_Test);
   -- Test routines:
   procedure Test_Wait_For_Bus_Or_Not (T : in out Simple_Test);
   procedure Test_Wait_For_Bus_Or_Not_Second_Stop (T : in out Simple_Test);
   procedure Test_Wait_For_Bus_Or_Not_Do_Not_Stop (T : in out Simple_Test);

   procedure Register_Tests (T: in out Simple_Test);
   overriding function Name (T: in Simple_Test) return AU.Message_String;

end Active.Traveller.Strategy.Simple.Tests;
