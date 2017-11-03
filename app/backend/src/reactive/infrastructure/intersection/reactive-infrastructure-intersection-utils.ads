with Reactive.District;

with Shared.Infra_Id_Set;

package Reactive.Infrastructure.Intersection.Utils is

   package Infra_Id_Set renames Shared.Infra_Id_Set;
   use Reactive.Infra_Id_Type;

   type Object (<>) is tagged limited private;
   type Reference is access all Intersection.Utils.Object'Class;

   function Get_Instance
     (District : access Reactive.District.Object'Class := null)
      return Intersection.Utils.Reference;

   procedure Find_Street_Direction
     (This : in Intersection.Utils.Object;
      Intersection_Id,
      Street_Id        : in     Infra_Id;
      Street_Direction :    out Direction.Cardinal;
      Found            :    out Boolean);

   function Find_Streets_Connected_With_Intersection
     (This            : in Intersection.Utils.Object;
      Intersection_Id : in Infra_Id) return Infra_Id_Set.Set;

private
   type Object is tagged limited record
      District : access Reactive.District.Object'Class;
   end record;

   Instance : Intersection.Utils.Reference;

end Reactive.Infrastructure.Intersection.Utils;
