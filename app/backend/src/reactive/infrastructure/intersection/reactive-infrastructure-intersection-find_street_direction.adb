separate (Reactive.Infrastructure.Intersection)
procedure Find_Street_Direction (
   This             :        Intersection.Object;
   Street_Id        : in     Infra_Id;
   Street_Direction :    out Direction.Cardinal;
   Found            :    out Boolean) is
begin
   Found := FALSE;
   for Direction in Shared.Direction.Cardinal
   loop
      if This.Streets_Existence (Direction)
        and then Street_Id = This.Streets (Direction) then
         Street_Direction := Direction;
         Found := TRUE;
         return;
      end if;
   end loop;
end Find_Street_Direction;