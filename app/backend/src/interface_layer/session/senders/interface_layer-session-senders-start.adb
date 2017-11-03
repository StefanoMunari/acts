separate (Interface_Layer.Session.Senders)
procedure Start is
begin
   Sender_State := PT.ACTIVE;
   Sender_Ref.all.Send;
end Start;
