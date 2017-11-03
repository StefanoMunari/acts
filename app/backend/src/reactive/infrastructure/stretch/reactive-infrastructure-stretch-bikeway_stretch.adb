package body Reactive.Infrastructure.Stretch.Bikeway_Stretch is

   function Create (
      Id              : in Infra_Id;
      Size            : in Natural;
      Host_Utils      : access Building.Host.Utils.Object'Class := null;
      Lane_Utils      : access Lane.Utils.Object'Class := null;
      Traveller_Utils : access Active.Traveller.Utils.Object'Class := null)
   return Stretch.Bikeway_Stretch.Reference
   is
      Bikeway_Stretch : Stretch.Bikeway_Stretch.Reference
        := new Infrastructure.Stretch.Bikeway_Stretch.Object;
   begin

      Bikeway_Stretch.Host_Utils := Host_Utils;
      if Bikeway_Stretch.Host_Utils = null then
         Bikeway_Stretch.Host_Utils := Building.Host.Utils.Get_Instance;
      end if;

      Stretch.Init (Stretch         => Bikeway_Stretch.all,
                    Id              => Id,
                    Size            => Size,
                    Lane_Utils      => Lane_Utils,
                    Traveller_Utils => Traveller_Utils);
      return Bikeway_Stretch;

   end Create;

   procedure Set_Host (
      This    : in out Bikeway_Stretch.Object;
      Host_Id : in     Infra_Id) is
   use Building.Host.Utils;
   begin
      Stretch.Set_Host (Stretch.Object (This), Host_Id);
      This.Host_Utils.Accessible_By (Host_Id, This.Get_Id, BIKE);
   end Set_Host;

end Reactive.Infrastructure.Stretch.Bikeway_Stretch;
