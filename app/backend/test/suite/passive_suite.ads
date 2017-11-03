with AUnit.Test_Suites;

package Passive_Suite is
   package TS renames AUnit.Test_Suites;
   function Suite return TS.Access_Test_Suite;
end Passive_Suite;
