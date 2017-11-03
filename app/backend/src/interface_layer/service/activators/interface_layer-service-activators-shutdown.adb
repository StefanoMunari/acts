separate (Interface_Layer.Service.Activators)
procedure Shutdown is
begin
   -- shutdown the Activators sublayer
   -- Requests_Pkg has already been termianted (by itself)
   -- Stop Acks so it can empty its queue and then terminate
   Acks_Pkg.Stop;
end Shutdown;
