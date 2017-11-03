separate (Interface_Layer.Session.Senders)
function Is_Shutdown (Action : String) return Boolean
is
   use PT; -- make '=' visible for Process_Types
begin
   if (Sender_State = PT.ACTIVE or Sender_State = PT.READY)
      and then Action = IL_Shutdown_Message
   then
      return True;
   end if;
   return False;
end Is_Shutdown;