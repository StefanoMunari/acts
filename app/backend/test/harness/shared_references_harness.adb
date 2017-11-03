with AUnit.Reporter.Text;
with AUnit.Run;

with Shared_References_Natural_Mock_Suite;

procedure Shared_References_Harness is
   procedure Runner is new AUnit.Run.Test_Runner
      (Shared_References_Natural_Mock_Suite.Suite);
   Reporter : AUnit.Reporter.Text.Text_Reporter;

   begin
      Runner (Reporter);
end Shared_References_Harness;
