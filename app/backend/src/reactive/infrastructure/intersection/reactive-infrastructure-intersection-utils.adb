with Reactive.District;
with Reactive.Infrastructure.Street;

package body Reactive.Infrastructure.Intersection.Utils is

   function Get_Instance (
      District : access Reactive.District.Object'Class := null)
   return Intersection.Utils.Reference is
   begin
      if Intersection.Utils.Instance = null then
         Intersection.Utils.Instance := new Intersection.Utils.Object;
      end if;

      if District = null then
         Instance.District := Reactive.District.Get_Instance;
      else
         Instance.District := District;
      end if;

      return Intersection.Utils.Instance;
   end Get_Instance;

   procedure Find_Street_Direction (
      This : in Intersection.Utils.Object;
      Intersection_Id,
      Street_Id        : in Infra_Id;
      Street_Direction :    out Direction.Cardinal;
      Found            :    out Boolean) is
   begin
      This.District
        .Find_Intersection_By_Id (Intersection_Id => Intersection_Id)
        .Find_Street_Direction (Street_Id        => Street_Id,
                                Street_Direction => Street_Direction,
                                Found            => Found);
   end Find_Street_Direction;

   function Find_Streets_Connected_With_Intersection (
      This            : in Intersection.Utils.Object;
      Intersection_Id : in Infra_Id) return Infra_Id_Set.Set is
   begin
      return This.District
        .Find_Intersection_By_Id (Intersection_Id => Intersection_Id)
        .Find_Streets_Connected_With_Intersection;
   end Find_Streets_Connected_With_Intersection;

end Reactive.Infrastructure.Intersection.Utils;
