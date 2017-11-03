with AUnit.Reporter.Text;
with AUnit.Run;
with Procedure_Call_Suite;

procedure Procedure_Call_Harness is
   procedure Runner is new AUnit.Run.Test_Runner 
      (Procedure_Call_Suite.Suite);
   Reporter : AUnit.Reporter.Text.Text_Reporter;
   
   begin
      Runner (Reporter);
end Procedure_Call_Harness;
