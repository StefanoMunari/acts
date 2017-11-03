separate (Reactive.Infrastructure.Intersection)
function Get_Street (This      : in     Intersection.Object;
                     Direction : in out Shared.Direction.Cardinal)
  return Infra_Id
is
begin
   return This.Streets (Direction);
end Get_Street;