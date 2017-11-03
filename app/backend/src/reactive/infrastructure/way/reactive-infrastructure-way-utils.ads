limited with Reactive.District;

package Reactive.Infrastructure.Way.Utils is

   type Object (<>) is tagged limited private;
   type Reference is access all Way.Utils.Object'Class;

   function Get_Instance (
      District : access Reactive.District.Object'Class := null)
   return Way.Utils.Reference;

   not overriding
   function Find_Street (This   : in Way.Utils.Object;
                         Way_Id : in Infra_Id)
   return Infra_Id;

   not overriding
   function Is_Contained_By (
      This         : in Way.Utils.Object;
      Way_Id       : in Infra_Id;
      Container_Id : in Infra_Id)
   return Boolean;

   not overriding
   procedure Find_Lane_By_Direction (
      This             : in     Way.Utils.Object;
      Way_Id           : in     Infra_Id;
      Travel_Direction : in     Direction.Straight;
      Lane_Id          :    out Infra_Id;
      Found            :    out Boolean);

   not overriding
   function Get_Stretches_On_Other_Lane (
      This         : Way.Utils.Object;
      Way_Id       : Infra_Id;
      Lane_Id      : Infra_Id;
      Stretch_List : Natural_List.List)
   return Infra_Id_List.List;

private
   type Object is tagged limited record
      District : access Reactive.District.Object'Class;
   end record;

   Instance : Way.Utils.Reference := null;

end Reactive.Infrastructure.Way.Utils;
