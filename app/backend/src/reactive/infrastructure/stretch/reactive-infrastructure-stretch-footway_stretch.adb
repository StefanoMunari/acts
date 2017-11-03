package body Reactive.Infrastructure.Stretch.Footway_Stretch is

   function Create (
      Id              : in Infra_Id;
      Size            : in Natural;
      Host_Utils      : access Building.Host.Utils.Object'Class := null;
      Lane_Utils      : access Lane.Utils.Object'Class := null;
      Traveller_Utils : access Active.Traveller.Utils.Object'Class := null)
   return Infrastructure.Stretch.Footway_Stretch.Reference
   is
      Footway_Stretch : Infrastructure.Stretch.Footway_Stretch.Reference
        := new Infrastructure.Stretch.Footway_Stretch.Object;
   begin

      Footway_Stretch.Host_Utils := Host_Utils;
      if Footway_Stretch.Host_Utils = null then
         Footway_Stretch.Host_Utils := Building.Host.Utils.Get_Instance;
      end if;

      Stretch.Init (Stretch         => Footway_Stretch.all,
                    Id              => Id,
                    Size            => Size,
                    Lane_Utils      => Lane_Utils,
                    Traveller_Utils => Traveller_Utils);
      return Footway_Stretch;

   end Create;

   procedure Set_Host (
      This    : in out Footway_Stretch.Object;
      Host_Id : in     Infra_Id) is
   begin
      Stretch.Set_Host (Stretch.Object (This), Host_Id);
      This.Host_Utils.Accessible_By (Host_Id, This.Get_Id, FOOT);
   end Set_Host;

end Reactive.Infrastructure.Stretch.Footway_Stretch;
