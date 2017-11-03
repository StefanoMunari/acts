with AUnit.Test_Suites;

package Shared_Suite is
   package TS renames AUnit.Test_Suites;
   function Suite return TS.Access_Test_Suite;
end Shared_Suite;
