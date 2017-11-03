with AUnit;
with AUnit.Test_Cases;

package Converter.Tests is
   package AU renames AUnit;
   package TC renames AUnit.Test_Cases;

   type Converter_Test is new 
      TC.Test_Case with null record;

   overriding procedure Set_Up_Case (T: in out Converter_Test);
   overriding procedure Tear_Down_Case (T: in out Converter_Test);
   overriding procedure Set_Up (T: in out Converter_Test);
   overriding procedure Tear_Down (T: in out Converter_Test);

   procedure Register_Tests (T: in out Converter_Test);
   function Name (T: in Converter_Test) return AU.Message_String;

   -- Test Routines:
   procedure Encode (T: in out TC.Test_Case'Class);
   procedure Decode (T: in out TC.Test_Case'Class);
end Converter.Tests;
