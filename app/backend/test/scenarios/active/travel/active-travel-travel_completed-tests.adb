with AUnit.Assertions;
with Ada.Text_IO;

with Active.People_Carrier.Utils.Mock;
with Active.Space_Master.Mock;
with Active.Travel.Mock;
with Active.Traveller.Utils.Mock;

with Reactive.Infrastructure.Building.Host.Utils.Mock;
with Reactive.Infrastructure.Stretch.Utils.Mock;

package body Active.Travel.Travel_Completed.Tests is

   package Ass renames AUnit.Assertions;
   package Space_Master_Mock renames Active.Space_Master.Mock;
   package Travel_Mock renames Active.Travel.Mock;
   package People_Carrier_Utils_Mock renames Active.People_Carrier.Utils.Mock;
   package Traveller_Utils_Mock renames Active.Traveller.Utils.Mock;
   package Host_Utils_Mock
      renames Reactive.Infrastructure.Building.Host.Utils.Mock;
   package Stretch_Utils_Mock
      renames Reactive.Infrastructure.Stretch.Utils.Mock;

   -----------------------------------------------------
   --                   SCAFFOLDING
   -----------------------------------------------------
   Travel : Travel_Mock.Reference;
   Travel_Completed : Active.Travel.Travel_Completed.Reference;
   People_Carrier_Utils : access People_Carrier_Utils_Mock.Object;
   Space_Master : access Space_Master_Mock.Object;
   Traveller_Utils : access Traveller_Utils_Mock.Object;
   Host_Utils : access Host_Utils_Mock.Object;
   Stretch_Utils : access Stretch_Utils_Mock.Object;

   procedure Set_Up (T: in out Travel_Completed_Test) is
   begin
      Travel := Travel_Mock.Create;
      Host_Utils := Host_Utils_Mock.Create;
      Traveller_Utils := Traveller_Utils_Mock.Create;
      Space_Master := Space_Master_Mock.Create;
      People_Carrier_Utils := People_Carrier_Utils_Mock.Create;
      Stretch_Utils := Stretch_Utils_Mock.Create;
      Travel_Completed := Active.Travel.Travel_Completed.Get_Instance (
         Host_Utils      => Host_Utils,
         Traveller_Utils => Traveller_Utils,
         Space_Master    => Space_Master,
         PC_Utils        => People_Carrier_Utils,
         Stretch_Utils   => Stretch_Utils);
   end Set_Up;

   procedure Test_Has_No_Next_Step (T: in out TC.Test_Case'Class) is
   begin
      Ass.Assert (not Travel_Completed.Has_Next_Step (Travel.all),
                  "The travel has next step");
   end Test_Has_No_Next_Step;

   procedure Test_Is_Not_Progressing (T: in out TC.Test_Case'Class) is
   begin
      Ass.Assert (not Travel_Completed.Is_Progressing (Travel.all),
                  "The travel is progressing");
   end Test_Is_Not_Progressing;

   -----------------------------------------------------
   --                  REGISTRATION
   -----------------------------------------------------
   procedure Register_Tests (T: in out Travel_Completed_Test) is
      use TC.Registration;
   begin
      Register_Routine (Test => T,
                        Routine => Test_Has_No_Next_Step'Access,
                        Name => "Test has not next step");

      Register_Routine
        (Test => T,
         Routine => Test_Is_Not_Progressing'Access,
         Name => "Test is not progressing");
   end Register_Tests;

   function Name(T: Travel_Completed_Test) return AU.Message_String is
   begin
      return AU.Format ("Travel_Completed");
   end Name;
end Active.Travel.Travel_Completed.Tests;
