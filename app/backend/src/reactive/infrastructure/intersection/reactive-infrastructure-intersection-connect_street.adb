separate (Reactive.Infrastructure.Intersection)
procedure Connect_Street(
   This      : in out Intersection.Object;
   Street_Id : in     Infra_Id;
   Stretches : in     Infra_Id_List.List;
   Direction : in     Shared.Direction.Cardinal) is
begin
   This.Streets (Direction)             := Street_Id;
   This.Streets_Existence (Direction)   := True;
   This.Stretches (Direction)           := Stretches;
   This.Stretches_Existence (Direction) := True;
end Connect_Street;
