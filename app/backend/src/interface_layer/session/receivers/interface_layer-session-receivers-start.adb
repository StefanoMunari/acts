separate (Interface_Layer.Session.Receivers)
procedure Start is
begin
   Receiver_State := PT.ACTIVE;
   Receiver_Ref.all.Listen;
end Start;
