separate (Reactive.Infrastructure.Intersection)
function Dump (This : in Intersection.Object) return G_JSON.JSON_Value
is
   JSON       : G_JSON.JSON_Value := G_JSON.Create_Object;
   Exits_JSON : G_JSON.JSON_Array := G_JSON.Empty_Array;
   Exit_JSON  : G_JSON.JSON_Value;
   Street_Id  : Infra_Id;
   TL_Id      : Agent.Agent_Id; -- traffic light id
begin
   JSON.Set_Field (Id_Field, Integer (This.Id));

   for Cardinal_Direction in Direction.Cardinal loop
      if This.Streets_Existence (Cardinal_Direction) then
         Street_Id := This.Streets (Cardinal_Direction);
         TL_Id :=
            This.Crossing_Strategy.Get_Traffic_Light (Cardinal_Direction);
         Exit_JSON := G_JSON.Create_Object;
         Exit_JSON.Set_Field (Street_Id_Field, Integer (Street_Id));
         Exit_JSON.Set_Field (
            Direction_Field, Direction.Cardinal'Image (Cardinal_Direction));
         Exit_JSON.Set_Field (Traffic_Light_Field, TL_Id);
         if This.Entries.Has_Traveller_Id (Cardinal_Direction) then
            declare
               TR_Id_JSON  : G_JSON.JSON_Value; -- traveller id in json
            begin
               TR_Id_JSON :=
                  G_JSON.Create (
                     This.Entries.Get_Traveller_Id (Cardinal_Direction));
               Exit_JSON.Set_Field (Traveller_Field, TR_Id_JSON);
            end;
         end if;
         G_JSON.Append (Exits_JSON, Exit_JSON);
      end if;
   end loop;

   JSON.Set_Field (Exits_Field, Exits_JSON);

   return JSON;
end Dump;