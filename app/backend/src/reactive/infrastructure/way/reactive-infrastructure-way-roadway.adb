package body Reactive.Infrastructure.Way.Roadway is

   function Create (
      Id : in Infra_Id;
      Traveller_Utils : access Traveller.Utils.Object'Class := null;
      Street_Utils : access Street.Utils.Object'Class := null;
      Lane_Utils : access Lane.Utils.Object'Class := null)
                    return Infrastructure.Way.Roadway.Reference
   is
      Roadway : Infrastructure.Way.Roadway.Reference
        := new Infrastructure.Way.Roadway.Object;
   begin
      Way.Init (Way             => Roadway.all,
                Id              => Id,
                Traveller_Utils => Traveller_Utils,
                Street_Utils    => Street_Utils,
                Lane_Utils      => Lane_Utils);
      return Roadway;
   end Create;

end Reactive.Infrastructure.Way.Roadway;
