separate (Reactive.Infrastructure.Intersection)
function Find_Intersections (This : in Intersection.Object)
return Infra_Id_Set.Set
is
   Intersections : Infra_Id_Set.Set := Infra_Id_Set.Empty_Set;
begin
   Intersections.Insert (New_Item => This.Id);
   return Intersections;
end Find_Intersections;
