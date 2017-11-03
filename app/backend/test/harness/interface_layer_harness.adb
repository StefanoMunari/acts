with AUnit.Reporter.Text;
with AUnit.Run;

with Interface_Layer_Suite;

procedure Interface_Layer_Harness is
   procedure Runner is new AUnit.Run.Test_Runner
      (Interface_Layer_Suite.Suite);
   Reporter : AUnit.Reporter.Text.Text_Reporter;

   begin
      Runner (Reporter);
end Interface_Layer_Harness;
