with AUnit.Test_Suites;

package Active_Traveller_Suite is
   package TS renames AUnit.Test_Suites;
   function Suite return TS.Access_Test_Suite;
end Active_Traveller_Suite;
