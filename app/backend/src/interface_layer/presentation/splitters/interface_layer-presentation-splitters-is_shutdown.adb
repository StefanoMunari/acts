separate (Interface_Layer.Presentation.Splitters)
function Is_Shutdown (Action : String) return Boolean
is
   use PT; -- make '=' visible for Process_Types
begin
   if (Splitter_State = PT.ACTIVE or Splitter_State = PT.READY)
      and then Action = IL_Shutdown_Message
   then
      return True;
   end if;
   return False;
end Is_Shutdown;
