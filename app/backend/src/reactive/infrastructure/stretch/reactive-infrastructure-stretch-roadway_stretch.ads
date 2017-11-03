with Reactive.Infrastructure.Building.Host.Utils;

package Reactive.Infrastructure.Stretch.Roadway_Stretch is

   package Building renames Reactive.Infrastructure.Building;

   type Object is new Stretch.Object with private;
   type Reference is access all Roadway_Stretch.Object'Class;

   function Create (
      Id              : in     Infra_Id;
      Size            : in     Natural;
      Host_Utils      : access Building.Host.Utils.Object'Class := null;
      Lane_Utils      : access Lane.Utils.Object'Class := null;
      Traveller_Utils : access Active.Traveller.Utils.Object'Class := null)
   return Roadway_Stretch.Reference;

   overriding
   procedure Set_Host (
      This    : in out Roadway_Stretch.Object;
      Host_Id : in     Infra_Id);

private
   type Object is new Stretch.Object with record
      Host_Utils : access Building.Host.Utils.Object'Class := null;
   end record;

end Reactive.Infrastructure.Stretch.Roadway_Stretch;
