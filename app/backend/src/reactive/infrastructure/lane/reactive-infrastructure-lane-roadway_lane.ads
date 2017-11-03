with Reactive.Infrastructure.Lane;

package Reactive.Infrastructure.Lane.Roadway_Lane is

   type Object is new Lane.Object with null record;
   type Reference is access all Roadway_Lane.Object'Class;

   function Create (
      Id        : in Infra_Id;
      Direction : in Shared.Direction.Straight;
      Stretch_Utils : access Stretch.Utils.Object'Class := null;
      Way_Utils : access Way.Utils.Object'Class := null;
      Traveller_Utils : access Traveller.Utils.Object'Class := null)
         return Roadway_Lane.Reference;

end Reactive.Infrastructure.Lane.Roadway_Lane;
