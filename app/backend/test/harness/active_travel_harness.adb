with AUnit.Reporter.Text;
with AUnit.Run;

with Active_Travel_Suite;

procedure Active_Travel_Harness is
   procedure Runner is new AUnit.Run.Test_Runner
      (Active_Travel_Suite.Suite);
   Reporter : AUnit.Reporter.Text.Text_Reporter;

   begin
      Runner (Reporter);
end Active_Travel_Harness;
