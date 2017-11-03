with AUnit.Reporter.Text;
with AUnit.Run;

with Active_Suite;

procedure Active_Harness is
   procedure Runner is new AUnit.Run.Test_Runner
      (Active_Suite.Suite);
   Reporter : AUnit.Reporter.Text.Text_Reporter;

   begin
      Runner (Reporter);
end Active_Harness;
