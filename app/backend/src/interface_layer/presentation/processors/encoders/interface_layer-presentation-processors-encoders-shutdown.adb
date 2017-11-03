separate (Interface_Layer.Presentation.Processors.Encoders)
procedure Shutdown is
   use PT; -- make '/=' visible for Process_Types
begin
   -- Terminate Processor
   Encoder_State := PT.TERMINATED;
end Shutdown;