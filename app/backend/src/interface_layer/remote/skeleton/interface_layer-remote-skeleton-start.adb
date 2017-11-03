separate (Interface_Layer.Remote.Skeleton)
procedure Start (
  This    : in     Skeleton.Object;
  Request : in out Interface_Wrapper.Object) is
begin
   Scheduling.System.Start;
end Start;
