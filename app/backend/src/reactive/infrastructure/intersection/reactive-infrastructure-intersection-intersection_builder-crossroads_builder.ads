with Reactive.Infrastructure.Intersection.Intersection_Builder;

package Reactive.Infrastructure.Intersection.Intersection_Builder.Crossroads_Builder is

   type Object is new Intersection_Builder.Object with null record;
   type Reference is access all Crossroads_Builder.Object;

   function Create (
      Intersection_Id    : in     Infra_Id;
      District           : access Reactive.District.Object'Class := null;
      Street_Utils       : access Street.Utils.Object'Class := null;
      Intersection_Utils : access Intersection.Utils.Object'Class := null;
      Lane_Utils         : access Lane.Utils.Object'Class := null;
      Intersection       :
         access Infrastructure.Intersection.Object'Class := null;
      Traveller_Utils    : access Active.Traveller.Utils.Object'Class := null;
      Infrastructure_Utils :
         access Reactive.Infrastructure.Utils.Object'Class := null;
      Crossing_Strategy  :
         access Infrastructure.Intersection.Crossing.Object'Class := null)
   return Crossroads_Builder.Reference;

end Reactive.Infrastructure.Intersection.Intersection_Builder.Crossroads_Builder;
