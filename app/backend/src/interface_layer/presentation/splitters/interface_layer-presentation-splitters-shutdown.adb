separate (Interface_Layer.Presentation.Splitters)

procedure Shutdown is
   use PT; -- make '/=' visible for Process_Types
begin
   Splitter_State := PT.TERMINATED;
end Shutdown;
