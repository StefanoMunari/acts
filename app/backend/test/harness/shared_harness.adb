with AUnit.Reporter.Text;
with AUnit.Run;

with Shared_Suite;

procedure Shared_Harness is
   procedure Runner is new AUnit.Run.Test_Runner
      (Shared_Suite.Suite);
   Reporter : AUnit.Reporter.Text.Text_Reporter;

   begin
      Runner (Reporter);
end Shared_Harness;
