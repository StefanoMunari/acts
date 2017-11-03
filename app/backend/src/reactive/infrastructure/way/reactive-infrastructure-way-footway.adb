package body Reactive.Infrastructure.Way.Footway is

   function Create (
      Id : in Infra_Id;
      Traveller_Utils : access Traveller.Utils.Object'Class := null;
      Street_Utils : access Street.Utils.Object'Class := null;
      Lane_Utils : access Lane.Utils.Object'Class := null)
   return Infrastructure.Way.Footway.Reference
   is
      Footway : Infrastructure.Way.Footway.Reference
        := new Infrastructure.Way.Footway.Object;
   begin
      Way.Init (Way             => Footway.all,
                Id              => Id,
                Traveller_Utils => Traveller_Utils,
                Street_Utils    => Street_Utils,
                Lane_Utils      => Lane_Utils);
      return Footway;
   end Create;

end Reactive.Infrastructure.Way.Footway;
