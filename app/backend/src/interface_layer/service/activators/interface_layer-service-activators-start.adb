separate (Interface_Layer.Service.Activators)
procedure Start is
begin
   -- start the Activators sublayer
   Acks_Pkg.Start;
   Requests_Pkg.Start;
end Start;
