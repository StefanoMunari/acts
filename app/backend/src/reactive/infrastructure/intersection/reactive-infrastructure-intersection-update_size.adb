separate (Reactive.Infrastructure.Intersection)
procedure Update_Size (
   This              : in out Intersection.Object;
   Intersection_Type : in Intersection.Intersection_Type) is
begin
   case Intersection_Type is
   when T_JUNCTION
      => This.Size := T_JUNCTION_WAYS_NUM;
   when CROSSROADS
      => This.Size := CROSSROADS_WAYS_NUM;
   end case;
end Update_Size;