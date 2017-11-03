separate (Interface_Layer.Presentation.Processors)
procedure Shutdown is
   -- TODO: implement scoped_pointers
   procedure Free is new
      Ada.Unchecked_Deallocation
      (Base_Converter.Object'Class, Base_Converter.Reference);
begin
   -- Shutdown Encoders; Decoders has already been terminated (by itself)
   Encoders_Pkg.Stop;
   -- Free resources
   Free (Processors.Format_Converter);
end Shutdown;