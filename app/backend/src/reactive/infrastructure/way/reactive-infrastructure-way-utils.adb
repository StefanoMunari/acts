with Reactive.District;

package body Reactive.Infrastructure.Way.Utils is

   function Get_Instance (
      District : access Reactive.District.Object'Class := null)
   return Way.Utils.Reference is
   begin
      if Way.Utils.Instance = null then
         Way.Utils.Instance := new Way.Utils.Object;
      end if;

      if District = null then
         Instance.District := Reactive.District.Get_Instance;
      else
         Instance.District := District;
      end if;

      return Way.Utils.Instance;
   end Get_Instance;

   function Find_Street (This   : in Way.Utils.Object;
                         Way_Id : in Infra_Id)
   return Infra_Id is
   begin
      return This.District
                 .Find_Way_By_Id (Way_Id)
                 .Find_Street;
   end Find_Street;

   function Is_Contained_By (
      This         : in Way.Utils.Object;
      Way_Id       : in Infra_Id;
      Container_Id : in Infra_Id)
   return Boolean is
   begin
      return This.District
                 .Find_Way_By_Id (Way_Id)
                 .Is_Contained_By (Container_Id => Container_Id);
   end Is_Contained_By;

   not overriding
   procedure Find_Lane_By_Direction (
      This             : in     Way.Utils.Object;
      Way_Id           : in     Infra_Id;
      Travel_Direction : in     Direction.Straight;
      Lane_Id          :    out Infra_Id;
      Found            :    out Boolean)
   is
   begin
      This.District.Find_Way_By_Id (Way_Id)
         .Find_Lane_By_Direction(Travel_Direction, Lane_Id, Found);
   end Find_Lane_By_Direction;

   function Get_Stretches_On_Other_Lane (
      This         : Way.Utils.Object;
      Way_Id       : Infra_Id;
      Lane_Id      : Infra_Id;
      Stretch_List : Natural_List.List)
   return Infra_Id_List.List is
   begin
      return This.District
                 .Find_Way_By_Id (Way_Id)
                 .Get_Stretches_On_Other_Lane(Lane_Id, Stretch_List);
   end Get_Stretches_On_Other_Lane;

end Reactive.Infrastructure.Way.Utils;
