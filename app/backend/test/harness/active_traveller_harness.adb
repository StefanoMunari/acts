with AUnit.Reporter.Text;
with AUnit.Run;

with Active_Traveller_Suite;

procedure Active_Traveller_Harness is
   procedure Runner is new AUnit.Run.Test_Runner
      (Active_Traveller_Suite.Suite);
   Reporter : AUnit.Reporter.Text.Text_Reporter;

   begin
      Runner (Reporter);
end Active_Traveller_Harness;
