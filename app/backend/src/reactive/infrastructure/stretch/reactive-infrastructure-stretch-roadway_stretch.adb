package body Reactive.Infrastructure.Stretch.Roadway_Stretch is

   function Create (
      Id              : in Infra_Id;
      Size            : in Natural;
      Host_Utils      : access Building.Host.Utils.Object'Class := null;
      Lane_Utils      : access Lane.Utils.Object'Class := null;
      Traveller_Utils : access Active.Traveller.Utils.Object'Class := null)
   return Roadway_Stretch.Reference
   is
      Roadway_Stretch : Stretch.Roadway_Stretch.Reference
        := new Stretch.Roadway_Stretch.Object;
   begin

      Roadway_Stretch.Host_Utils := Host_Utils;
      if Roadway_Stretch.Host_Utils = null then
         Roadway_Stretch.Host_Utils := Building.Host.Utils.Get_Instance;
      end if;

      Stretch.Init (Stretch         => Roadway_Stretch.all,
                    Id              => Id,
                    Size            => Size,
                    Lane_Utils      => Lane_Utils,
                    Traveller_Utils => Traveller_Utils);
      return Roadway_Stretch;

   end Create;

   procedure Set_Host (
      This    : in out Roadway_Stretch.Object;
      Host_Id : in     Infra_Id) is
   begin
      Stretch.Set_Host (Stretch.Object (This), Host_Id);
      This.Host_Utils.Accessible_By (Host_Id, This.Get_Id, ROAD);
   end Set_Host;

end Reactive.Infrastructure.Stretch.Roadway_Stretch;
