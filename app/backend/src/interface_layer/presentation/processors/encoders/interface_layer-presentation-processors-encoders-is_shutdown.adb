separate (Interface_Layer.Presentation.Processors.Encoders)
function Is_Shutdown (Action : String) return Boolean
is
   use PT; -- make '=' visible for Process_Types
begin
   if (Encoder_State = PT.ACTIVE or Encoder_State = PT.READY)
      and then Action = IL_Shutdown_Message
   then
      return True;
   end if;
   return False;
end Is_Shutdown;