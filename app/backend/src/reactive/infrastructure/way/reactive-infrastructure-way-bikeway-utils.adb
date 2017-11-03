with Reactive.District;

package body Reactive.Infrastructure.Way.Bikeway.Utils is

   function Get_Instance (
      District : access Reactive.District.Object'Class := null)
   return Bikeway.Utils.Reference is
   begin
      if Bikeway.Utils.Instance = null then
         Bikeway.Utils.Instance := new Bikeway.Utils.Object;
      end if;

      if District = null then
         Instance.District := Reactive.District.Get_Instance;
      else
         Instance.District := District;
      end if;

      return Bikeway.Utils.Instance;
   end Get_Instance;

   not overriding
   procedure Set_Street (This       : in Bikeway.Utils.Object;
                         Bikeway_Id : in Infra_Id;
                         Street_Id  : in Infra_Id)
   is
   begin
      This.District.Find_Bikeway_By_Id (Bikeway_Id)
                   .Set_Street (Street_Id);
   end Set_Street;

   procedure Find_Lane_By_Direction (
      This             : in Bikeway.Utils.Object;
      Bikeway_Id       : in Infra_Id;
      Travel_Direction : in Direction.Straight;
      Lane_Id          : out Infra_Id;
      Found            : out Boolean) is
   begin
      This.District
        .Find_Bikeway_By_Id (Bikeway_Id)
        .Find_Lane_By_Direction (Travel_Direction => Travel_Direction,
                                 Lane_Id          => Lane_Id,
                                 Found            => Found);
   end Find_Lane_By_Direction;

   procedure Validate (This       : in Bikeway.Utils.Object;
                       Bikeway_Id : in Infra_Id)
   is
   begin
      This.District.Find_Bikeway_By_Id (Bikeway_Id).Validate;
   end Validate;

   function Dump (This : Bikeway.Utils.Object; Bikeway_Id : in Infra_Id)
   return G_JSON.JSON_Value is
   begin
     return This.District
                .Find_Bikeway_By_Id (Bikeway_Id)
                .Dump;
   end Dump;

end Reactive.Infrastructure.Way.Bikeway.Utils;
