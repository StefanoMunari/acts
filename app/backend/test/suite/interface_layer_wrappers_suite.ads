with AUnit.Test_Suites;

package Interface_Layer_Wrappers_Suite is
   package TS renames AUnit.Test_Suites;
   function Suite return TS.Access_Test_Suite;
end Interface_Layer_Wrappers_Suite;
