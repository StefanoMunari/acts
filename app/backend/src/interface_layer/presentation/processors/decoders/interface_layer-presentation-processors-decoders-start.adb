separate (Interface_Layer.Presentation.Processors.Decoders)
procedure Start is
begin
   Decoder_State := PT.ACTIVE;
   Decoder_Ref.all.Decode;
end Start;
