separate (Interface_Layer.Service.Activators.Requests)
function Is_Shutdown (Action : String) return Boolean
is
   use PT; -- make '=' visible for Process_Types
begin
   If (Request_State = PT.ACTIVE or Request_State = PT.READY)
      and then Action = IL_Shutdown_Message
   then
      return True;
   end if;
   return False;
end Is_Shutdown;
