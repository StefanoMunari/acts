separate (Interface_Layer.Service.Activators.Acks)
procedure Start is
begin
   Ack_State := PT.ACTIVE;
   Ack_Ref.all.Active;
end Start;
