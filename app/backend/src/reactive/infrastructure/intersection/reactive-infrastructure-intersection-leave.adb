separate (Reactive.Infrastructure.Intersection)
procedure Leave (
   This         : in out Intersection.Object;
   Traveller_Id : in     Agent.Agent_Id;
   Left         :    out Boolean)
is
begin
   This.Entries.Leave (Traveller_Id);
   Left := True;
end Leave;
