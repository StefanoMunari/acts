separate (Interface_Layer.Service.Activators.Requests)
procedure Start is
begin
   Request_State := PT.ACTIVE;
   Request_Ref.all.Active;
end Start;
