with Reactive.Infrastructure.Way;

package Reactive.Infrastructure.Way.Footway is

   type Object is new Way.Object with null record;
   type Reference is access all Footway.Object'Class;

   function Create (
      Id : in Infra_Id;
      Traveller_Utils : access Traveller.Utils.Object'Class := null;
      Street_Utils : access Street.Utils.Object'Class := null;
      Lane_Utils : access Lane.Utils.Object'Class := null)
   return Infrastructure.Way.Footway.Reference;

end Reactive.Infrastructure.Way.Footway;
