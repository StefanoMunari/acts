separate (Interface_Layer.Session.Receivers)
procedure Shutdown is
   use PT; -- make '/=' visible for Process_Types
begin
   Receiver_State := PT.TERMINATED;
-- Free resources
-- Close the socket associated with the listening server
   G_Socket.Close_Socket (Server);
-- Call when GNAT.Sockets are closed
   G_Socket.Finalize;
end Shutdown;