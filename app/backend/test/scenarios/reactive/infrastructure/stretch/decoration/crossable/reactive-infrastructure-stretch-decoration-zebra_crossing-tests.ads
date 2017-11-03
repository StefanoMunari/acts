with AUnit;
with AUnit.Test_Cases;

with Active.Agent;
with Active.Traveller;
with Active.Traveller.Utils.Mock;

with Reactive.District;
with Reactive.Infrastructure.Stretch.Mock;

package Reactive.Infrastructure.Stretch.Decoration.Zebra_Crossing.Tests is
   package AU renames AUnit;
   package TC renames AUnit.Test_Cases;
   package Agent renames Active.Agent;
   package Traveller_Utils_Mock_Pkg renames Active.Traveller.Utils.Mock;
   package District renames Reactive.District;
   package Stretch_Mock renames Reactive.Infrastructure.Stretch.Mock;

   type Zebra_Crossing_Test is abstract new TC.Test_Case with record
      Crossing        : Zebra_Crossing.Reference;
      Stretch_Ref     : Stretch_Mock.Reference;
      District_Ref    : District.Reference;
      Privileged      : Active.Traveller.Reference;
      Privileged_Id   : Agent.Agent_Id;
      Unprivileged    : Active.Traveller.Reference;
      Unprivileged_Id : Agent.Agent_Id;
      Trav_Queue      : access Protected_Travellers_Queue;
      Trav_Utils      : Traveller_Utils_Mock_Pkg.Reference;
   end record;

   overriding
   procedure Set_Up (T : in out Zebra_Crossing_Test);
   overriding
   procedure Tear_Down (T : in out Zebra_Crossing_Test);

   -- Test Routines:
   procedure Test_Tread_With_Free_Stretch (T : in out Zebra_Crossing_Test);
   procedure Test_Tread_And_Then_Leave (T : in out Zebra_Crossing_Test);
   procedure Test_Privileged_Waits_For_Unprivileged
      (T : in out Zebra_Crossing_Test);
   procedure Test_Unprivileged_Does_Not_Wait_For_Privileged
      (T : in out Zebra_Crossing_Test);
   procedure Test_Privileged_Treads_And_Leaves
      (T : in out Zebra_Crossing_Test);
   procedure Test_Unprivileged_Treads_And_Leaves
      (T : in out Zebra_Crossing_Test);
   procedure Test_Privileged_Have_Right_Of_Way
      (T : in out Zebra_Crossing_Test);
   procedure Test_Unprivileged_Do_Not_Have_Right_Of_Way
      (T : in out Zebra_Crossing_Test);

   procedure Register_Tests (T : in out Zebra_Crossing_Test);

private
   procedure Tread (T            : in out Zebra_Crossing_Test;
                    Traveller_Id : in Agent.Agent_Id;
                    Advanced     : out Boolean);

   procedure Leave (T            : in out Zebra_Crossing_Test;
                    Traveller_Id : in     Agent.Agent_Id;
                    Left         :    out Boolean);

end Reactive.Infrastructure.Stretch.Decoration.Zebra_Crossing.Tests;
