separate (Interface_Layer.Presentation.Processors.Encoders)
procedure Start is
begin
   Encoder_State := PT.ACTIVE;
   Encoder_Ref.all.Encode;
end Start;
