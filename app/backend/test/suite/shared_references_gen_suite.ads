with AUnit.Test_Caller;
with AUnit.Test_Suites; use AUnit.Test_Suites;

with Shared.Natural_Mock.Tests;
use Shared.Natural_Mock.Tests;

generic
   Instance_Name : String;
package Shared_References_Gen_Suite is
   package TS renames AUnit.Test_Suites;
   package Caller is new AUnit.Test_Caller
      (Shared.Natural_Mock.Tests.Shared_References_Test);

   use TS;
   function Suite return Access_Test_Suite;
end Shared_References_Gen_Suite;
