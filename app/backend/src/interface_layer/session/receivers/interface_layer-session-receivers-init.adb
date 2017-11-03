separate (Interface_Layer.Session.Receivers)
procedure Init (
   Hostname       : String;
   Port           : G_Socket.Port_Type;
   Task_Pool_Size : Stacks.Stack_Range)
is
begin
   Stack_Instance.Init (Task_Pool_Size);
   Address.Addr := G_Socket.Any_Inet_Addr;
   Address.Port := Port;
   G_Socket.Create_Socket (Socket => Server);
   G_Socket.Set_Socket_Option (
      Socket => Server,
      Option => (Name    => G_Socket.Reuse_Address, Enabled => True));
   G_Socket.Bind_Socket (
      Socket  => Server,
      Address => (Family => G_Socket.Family_Inet,
                  Addr   => Address.Addr,
                  Port   => Address.Port)
   );
   G_Socket.Listen_Socket (Socket => Server);
-- the receiver task is now ready to execute
   Receiver_State := PT.READY;
-- create the receiver task
   Receiver_Ref := new Receiver;
end Init;