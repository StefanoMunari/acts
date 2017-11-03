with AUnit;
with AUnit.Test_Cases;

with Active.Agent;
with Active.Traveller.Utils.Mock;

with Reactive.Infrastructure.Building.Host.Utils.Mock;
with Reactive.Infrastructure.Lane.Utils.Mock;

package Reactive.Infrastructure.Stretch.Tests is
   package AU renames AUnit;
   package TC renames AUnit.Test_Cases;
   package Agent renames Active.Agent;

   type Stretch_Test is abstract new TC.Test_Case with record
      Stretch : Infrastructure.Stretch.Reference;
      Host_Utils : Reactive.Infrastructure.Building.Host.Utils.Mock.Reference;
      Lane_Utils : Reactive.Infrastructure.Lane.Utils.Mock.Reference;
      Traveller_Utils : Active.Traveller.Utils.Mock.Reference;
   end record;

   overriding procedure Tear_Down (T: in out Stretch_Test);

   -- Test Routines:
   procedure Test_Tread_When_Stretch_Not_Full (T : in out Stretch_Test);
   procedure Test_Tread_When_Stretch_Full (T : in out Stretch_Test);
   procedure Test_No_Tread_When_Already_Inside (T : in out Stretch_Test);
   procedure Test_Tread_After_Exit_From_Waiting_List (T : in out Stretch_Test);
   procedure Test_Leave_Stretch (T : in out Stretch_Test);
   procedure Test_Find_Street (T : in out Stretch_Test);
   procedure Test_Intersections_Finder (T : in out Stretch_Test);
   procedure Test_Find_Lane (T : in out Stretch_Test);
   procedure Test_Stretch_Id_Getter (T : in out Stretch_Test);
   procedure Test_Lane_Setter (T : in out Stretch_Test);
   procedure Test_Is_Before_Into_The_Same_Lane (T : in out Stretch_Test);
   procedure Test_Is_After_Into_The_Same_Lane (T : in out Stretch_Test);
   procedure Test_Is_Before_Not_Into_The_Same_Lane (T : in out Stretch_Test);
   procedure Test_Is_Not_Before_Not_Into_The_Same_Lane (
      T : in out Stretch_Test);
   procedure Test_A_Stretch_Is_Not_Before_Itself (T : in out Stretch_Test);
   procedure Test_Is_Waiting_To_Enter_Stretch (T : in out Stretch_Test);
   procedure Test_Is_Not_Waiting_To_Enter_Stretch (T : in out Stretch_Test);
   procedure Test_Host_Setter (T : in out Stretch_Test);
   procedure Test_Host_Getter (T : in out Stretch_Test);
   procedure Test_Has_Host (T : in out Stretch_Test);

   procedure Register_Tests (T : in out Stretch_Test);

private
   procedure Tread (T            : in out Stretch_Test;
                    Traveller_Id : in     Agent.Agent_Id;
                    Advanced     :    out Boolean);

   procedure Leave (T            : in out Stretch_Test;
                    Traveller_Id : in     Agent.Agent_Id;
                    Left         :    out Boolean);

   procedure Is_Before (T               : in out Stretch_Test;
                        Other_Stretch   : in     Stretch.Object'Class;
                        Stretches_Count : in     Natural;
                        Before          :    out Boolean);

   procedure Fill_Stretch (T: in out Stretch_Test);

end Reactive.Infrastructure.Stretch.Tests;
