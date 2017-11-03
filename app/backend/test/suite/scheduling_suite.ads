with AUnit.Test_Suites;

package Scheduling_Suite is
   package TS renames AUnit.Test_Suites;
   function Suite return TS.Access_Test_Suite;
end Scheduling_Suite;
