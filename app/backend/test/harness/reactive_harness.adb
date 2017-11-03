with AUnit.Reporter.Text;
with AUnit.Run;

with Reactive_Suite;

procedure Reactive_Harness is
   procedure Runner is new AUnit.Run.Test_Runner
      (Reactive_Suite.Suite);
   Reporter : AUnit.Reporter.Text.Text_Reporter;

   begin
      Runner (Reporter);
end Reactive_Harness;
