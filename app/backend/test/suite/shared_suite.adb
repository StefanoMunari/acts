with Shared.Direction.Tests;
with Shared.Atomics.Tests;
with Shared.Rendezvous.Boolean_Rendezvous.Tests;

package body Shared_Suite is

   Result                  : aliased TS.Test_Suite;
   Shared0                 : aliased Shared.Direction.Tests.Direction_Test;
   Atomics_Test            : aliased Shared.Atomics.Tests.Atomics_Test;
   Boolean_Rendezvous_Test : aliased Shared.Rendezvous.Boolean_Rendezvous.Tests.BR_Test;

   function Suite
      return TS.Access_Test_Suite is
   begin
      TS.Add_Test (Result'Access, Shared0'Access);
      TS.Add_Test (Result'Access, Atomics_Test'Access);
      TS.Add_Test (Result'Access, Boolean_Rendezvous_Test'Access);

      return Result'Access;
   end Suite;
end Shared_Suite;
