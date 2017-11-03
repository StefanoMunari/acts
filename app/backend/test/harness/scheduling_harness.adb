with AUnit.Reporter.Text;
with AUnit.Run;

with Scheduling_Suite;

procedure Scheduling_Harness is
   procedure Runner is new AUnit.Run.Test_Runner
      (Scheduling_Suite.Suite);
   Reporter : AUnit.Reporter.Text.Text_Reporter;

   begin
      Runner (Reporter);
end Scheduling_Harness;
