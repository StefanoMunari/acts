package body Reactive.Infrastructure.Lane.Footway_Lane is

   function Create (
      Id              : in Infra_Id;
      Direction       : in Shared.Direction.Straight;
      Stretch_Utils   : access Stretch.Utils.Object'Class := null;
      Way_Utils       : access Way.Utils.Object'Class := null;
      Traveller_Utils : access Traveller.Utils.Object'Class := null)
         return Footway_Lane.Reference
   is
      Footway_Lane : Infrastructure.Lane.Footway_Lane.Reference
        := new Infrastructure.Lane.Footway_Lane.Object;
   begin
      Lane.Init (Lane          => Footway_Lane.all,
                 Id            => Id,
                 Direction     => Direction,
                 Stretch_Utils => Stretch_Utils,
                 Way_Utils     => Way_Utils,
                 Traveller_Utils  => Traveller_Utils);
      return Footway_Lane;
   end Create;

end Reactive.Infrastructure.Lane.Footway_Lane;
