separate (Interface_Layer.Presentation.Processors)
procedure Start is
begin
   -- Start the processors
   Encoders_Pkg.Start;
   Decoders_Pkg.Start;
end Start;
