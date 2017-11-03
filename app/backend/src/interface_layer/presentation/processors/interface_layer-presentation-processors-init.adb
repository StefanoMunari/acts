separate (Interface_Layer.Presentation.Processors)
procedure Init (Format_Reference : Base_Converter.Reference) is
begin
   -- Configure the chosen Converter
   Processors.Format_Converter := Format_Reference;
   -- Init the processors
   Encoders_Pkg.Init;
   Decoders_Pkg.Init;
end Init;