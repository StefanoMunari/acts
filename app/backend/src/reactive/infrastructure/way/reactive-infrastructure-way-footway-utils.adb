with Reactive.District;

package body Reactive.Infrastructure.Way.Footway.Utils is

   function Get_Instance (
      District : access Reactive.District.Object'Class := null)
   return Footway.Utils.Reference is
   begin
      if Footway.Utils.Instance = null then
         Footway.Utils.Instance := new Footway.Utils.Object;
      end if;

      if District = null then
         Instance.District := Reactive.District.Get_Instance;
      else
         Instance.District := District;
      end if;

      return Footway.Utils.Instance;
   end Get_Instance;

   not overriding
   procedure Set_Street (This       : in Footway.Utils.Object;
                         Footway_Id : in Infra_Id;
                         Street_Id  : in Infra_Id)
   is
   begin
      This.District.Find_Footway_By_Id (Footway_Id)
                   .Set_Street (Street_Id);
   end Set_Street;

   procedure Find_Lane_By_Direction (
      This             : in Footway.Utils.Object;
      Footway_Id       : in Infra_Id;
      Travel_Direction : in Direction.Straight;
      Lane_Id          : out Infra_Id;
      Found            : out Boolean) is
   begin
      This.District
        .Find_Footway_By_Id (Footway_Id)
        .Find_Lane_By_Direction (Travel_Direction => Travel_Direction,
                                 Lane_Id          => Lane_Id,
                                 Found            => Found);
   end Find_Lane_By_Direction;

   procedure Validate (This       : in Footway.Utils.Object;
                       Footway_Id : in Infra_Id)
   is
   begin
      This.District.Find_Footway_By_Id (Footway_Id).Validate;
   end Validate;

   function Dump (This : Footway.Utils.Object; Footway_Id : in Infra_Id)
   return G_JSON.JSON_Value is
   begin
     return This.District
                .Find_Footway_By_Id (Footway_Id)
                .Dump;
   end Dump;

end Reactive.Infrastructure.Way.Footway.Utils;
