separate (Reactive.Infrastructure.Intersection)
procedure Initialize (This : in out Intersection.Object) is
begin
  This.Entries := new Protected_Entries;
  for Direction in Shared.Direction.Cardinal
  loop
     This.Streets_Existence (Direction) := FALSE;
  end loop;
end Initialize;