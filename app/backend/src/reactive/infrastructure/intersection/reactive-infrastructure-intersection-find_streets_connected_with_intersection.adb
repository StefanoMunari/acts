separate (Reactive.Infrastructure.Intersection)
function Find_Streets_Connected_With_Intersection (
  This : in Intersection.Object) return Infra_Id_Set.Set
is
  Connected_Streets : Infra_Id_Set.Set;
  Street_Id         : Infra_Id;
  Added             : Boolean;
begin
  for Direction in Shared.Direction.Cardinal
  loop
     Added := FALSE;
     if This.Streets_Existence (Direction) then

        Street_Id := This.Streets (Direction);

        if not Connected_Streets.Contains (Street_Id) then
           Connected_Streets
             .Insert (Street_Id);
           Added := Connected_Streets.Contains (Street_Id);
        end if;

     end if;
  end loop;
  return Connected_Streets;
end Find_Streets_Connected_With_Intersection;