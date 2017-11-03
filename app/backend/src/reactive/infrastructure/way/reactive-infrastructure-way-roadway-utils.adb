with Reactive.District;

package body Reactive.Infrastructure.Way.Roadway.Utils is

   function Get_Instance (
      District : access Reactive.District.Object'Class := null)
   return Roadway.Utils.Reference is
   begin
      if Roadway.Utils.Instance = null then
         Roadway.Utils.Instance := new Roadway.Utils.Object;
      end if;

      if District = null then
         Instance.District := Reactive.District.Get_Instance;
      else
         Instance.District := District;
      end if;

      return Roadway.Utils.Instance;
   end Get_Instance;

   not overriding
   procedure Set_Street (This       : in Roadway.Utils.Object;
                         Roadway_Id : in Infra_Id;
                         Street_Id  : in Infra_Id)
   is
   begin
      This.District.Find_Roadway_By_Id (Roadway_Id)
                   .Set_Street (Street_Id);
   end Set_Street;

   procedure Find_Lane_By_Direction (
      This       : in Roadway.Utils.Object;
      Roadway_Id       : in Infra_Id;
      Travel_Direction : in Direction.Straight;
      Lane_Id          : out Infra_Id;
      Found            : out Boolean) is
   begin
      This.District
        .Find_Roadway_By_Id (Roadway_Id)
        .Find_Lane_By_Direction (Travel_Direction => Travel_Direction,
                                 Lane_Id          => Lane_Id,
                                 Found            => Found);
   end Find_Lane_By_Direction;

   procedure Validate (This       : in Roadway.Utils.Object;
                       Roadway_Id : in Infra_Id)
   is
   begin
      This.District.Find_Roadway_By_Id (Roadway_Id).Validate;
   end Validate;

   function Dump (This : Roadway.Utils.Object; Roadway_Id : in Infra_Id)
   return G_JSON.JSON_Value is
   begin
     return This.District
                .Find_Roadway_By_Id (Roadway_Id)
                .Dump;
   end Dump;

end Reactive.Infrastructure.Way.Roadway.Utils;
