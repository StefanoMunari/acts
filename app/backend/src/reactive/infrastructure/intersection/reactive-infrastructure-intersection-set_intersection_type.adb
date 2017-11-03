separate (Reactive.Infrastructure.Intersection)
procedure Set_Intersection_Type (
   This              : in out Intersection.Object;
   Intersection_Type : in     Intersection.Intersection_Type) is
begin
   This.Intersection_Type := Intersection_Type;
   This.Update_Size (Intersection_Type => Intersection_Type);
end Set_Intersection_Type;
