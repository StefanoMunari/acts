package body Reactive.Infrastructure.Intersection.Intersection_Builder.T_Junction_Builder is

   package Intersection_Builder
   renames Reactive.Infrastructure.Intersection.Intersection_Builder;

   function Create (
      Intersection_Id      : in Infra_Id;
      District             : access Reactive.District.Object'Class := null;
      Street_Utils         : access Street.Utils.Object'Class := null;
      Intersection_Utils   : access Intersection.Utils.Object'Class := null;
      Lane_Utils           : access Lane.Utils.Object'Class := null;
      Intersection         :
         access Infrastructure.Intersection.Object'Class := null;
      Traveller_Utils      : access Active.Traveller.Utils.Object'Class := null;
      Infrastructure_Utils :
         access Reactive.Infrastructure.Utils.Object'Class := null;
      Crossing_Strategy    :
         access Infrastructure.Intersection.Crossing.Object'Class := null)
   return T_Junction_Builder.Reference
   is
      Builder : T_Junction_Builder.Reference
         := new T_Junction_Builder.Object;
   begin
      Intersection_Builder
        .Init (Builder              => Builder.all,
               Intersection_Id      => Intersection_Id,
               Intersection_Type    => Infrastructure.Intersection.T_JUNCTION,
               District             => District,
               Street_Utils         => Street_Utils,
               Intersection_Utils   => Intersection_Utils,
               Lane_Utils           => Lane_Utils,
               Intersection         => Intersection,
               Traveller_Utils      => Traveller_Utils,
               Infrastructure_Utils => Infrastructure_Utils,
               Crossing_Strategy    => Crossing_Strategy);
      return Builder;
   end Create;

end Reactive.Infrastructure.Intersection.Intersection_Builder.T_Junction_Builder;
