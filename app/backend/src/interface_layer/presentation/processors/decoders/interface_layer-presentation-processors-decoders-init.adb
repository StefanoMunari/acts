separate (Interface_Layer.Presentation.Processors.Decoders)
procedure Init is
begin
   -- Processor is ready to execute
   Decoder_State := PT.READY;
   -- create the task
   Decoder_Ref := new Decoder;
end Init;