separate (Interface_Layer.Session.Receivers)
function Is_Shutdown (Action : String) return Boolean
is
   use PT; -- make '=' visible for Process_Types
   Delimiter : String (1 .. 1) := ( 1 => '"');
   Result : String :=
      Shared.String_Splitter.Filter_Delimiters (Action, Delimiter);
begin
   if (Receiver_State = PT.ACTIVE or Receiver_State = PT.READY)
      and then Result = "" & IL_Shutdown_Message & ""
   then
      return True;
   end if;
   return False;
end Is_Shutdown;