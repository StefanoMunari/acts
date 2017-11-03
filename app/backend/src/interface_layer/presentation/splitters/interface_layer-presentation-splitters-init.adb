separate (Interface_Layer.Presentation.Splitters)

procedure Init is
begin
   -- the splitter task is now ready to execute
   Splitter_State := PT.READY;
   -- create the splitter task
   Splitter_Ref := new Splitter;
end Init;
