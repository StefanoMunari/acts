with Active.Agent;

limited with Reactive.District;

package Reactive.Infrastructure.Way.Footway.Utils is

   package Agent renames Active.Agent;

   type Object (<>) is tagged limited private;
   type Reference is access all Footway.Utils.Object'Class;

   function Get_Instance (
      District : access Reactive.District.Object'Class := null)
      return Footway.Utils.Reference;

   not overriding
   procedure Set_Street (This       : in Footway.Utils.Object;
                         Footway_Id : in Infra_Id;
                         Street_Id  : in Infra_Id);

   not overriding
   procedure Find_Lane_By_Direction (
      This             : in Footway.Utils.Object;
      Footway_Id       : in Infra_Id;
      Travel_Direction : in Direction.Straight;
      Lane_Id          : out Infra_Id;
      Found            : out Boolean);

   not overriding
   procedure Validate (This       : in Footway.Utils.Object;
                       Footway_Id : in Infra_Id);

   function Dump (This : Footway.Utils.Object; Footway_Id : in Infra_Id)
   return G_JSON.JSON_Value;

private
   type Object is tagged limited record
      District : access Reactive.District.Object'Class;
   end record;

   Instance : Footway.Utils.Reference := null;

end Reactive.Infrastructure.Way.Footway.Utils;
