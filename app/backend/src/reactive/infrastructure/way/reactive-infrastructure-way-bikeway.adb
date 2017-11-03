package body Reactive.Infrastructure.Way.Bikeway is

   function Create (
      Id : in Infra_Id;
      Traveller_Utils : access Traveller.Utils.Object'Class := null;
      Street_Utils : access Street.Utils.Object'Class := null;
      Lane_Utils : access Lane.Utils.Object'Class := null)
   return Infrastructure.Way.Bikeway.Reference
   is
      Bikeway : Infrastructure.Way.Bikeway.Reference
        := new Infrastructure.Way.Bikeway.Object;
   begin
      Way.Init (Way             => Bikeway.all,
                Id              => Id,
                Traveller_Utils => Traveller_Utils,
                Street_Utils    => Street_Utils,
                Lane_Utils      => Lane_Utils);
      return Bikeway;
   end Create;

end Reactive.Infrastructure.Way.Bikeway;
