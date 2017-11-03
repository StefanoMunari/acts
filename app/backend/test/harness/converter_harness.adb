with AUnit.Reporter.Text;
with AUnit.Run;
with Converter_Suite;

procedure Converter_Harness is
   procedure Runner is new AUnit.Run.Test_Runner 
      (Converter_Suite.Suite);
   Reporter : AUnit.Reporter.Text.Text_Reporter;
   
   begin
      Runner (Reporter);
end Converter_Harness;
