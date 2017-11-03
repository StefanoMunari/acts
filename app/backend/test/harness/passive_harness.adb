with AUnit.Reporter.Text;
with AUnit.Run;

with Passive_Suite;

procedure Passive_Harness is
   procedure Runner is new AUnit.Run.Test_Runner
      (Passive_Suite.Suite);
   Reporter : AUnit.Reporter.Text.Text_Reporter;

   begin
      Runner (Reporter);
end Passive_Harness;
