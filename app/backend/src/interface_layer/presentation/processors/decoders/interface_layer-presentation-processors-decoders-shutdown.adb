separate (Interface_Layer.Presentation.Processors.Decoders)
procedure Shutdown is
   use PT; -- make '/=' visible for Process_Types
begin
   -- Terminate Processor
   Decoder_State := PT.TERMINATED;
end Shutdown;