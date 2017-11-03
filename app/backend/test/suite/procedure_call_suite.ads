with AUnit.Test_Suites;

package Procedure_Call_Suite is
   package TS renames AUnit.Test_Suites;
   function Suite return TS.Access_Test_Suite;
end Procedure_Call_Suite;
