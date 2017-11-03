separate (Interface_Layer.Remote.Skeleton)
procedure Shutdown (This    : in     Skeleton.Object;
                    Request : in out Interface_Wrapper.Object)
is

   -- TODO: implement pipeline construction
   Result : Boolean := False;

begin
   -- Shutdown Interface Layer
   Interface_Layer.System.Shutdown;
   -- Shutdown Application Layer
   Result := Scheduling.System.Shutdown;
   -- Wrap result
   -- Result_Wrapper := This.Wrapper_Factory.Create_Wrapper (Result);
   Active.Travel.Remove_AI;
end Shutdown;
