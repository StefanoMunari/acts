separate (Interface_Layer.Presentation.Processors.Encoders)
procedure Init is
begin
   -- Processor is ready to execute
   Encoder_State := PT.READY;
   -- create the task
   Encoder_Ref := new Encoder;
end Init;