separate (Interface_Layer.Presentation.Processors.Decoders)
function Is_Shutdown (Action : String) return Boolean
is
   use PT; -- make '=' visible for Process_Types
begin
   if (Decoder_State = PT.ACTIVE or Decoder_State = PT.READY)
      and then Action = IL_Shutdown_Message
   then
      return True;
   end if;
   return False;
end Is_Shutdown;
