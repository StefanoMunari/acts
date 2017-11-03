with Active.Agent;

limited with Reactive.District;

package Reactive.Infrastructure.Way.Bikeway.Utils is

   package Agent renames Active.Agent;

   type Object (<>) is tagged limited private;
   type Reference is access all Bikeway.Utils.Object'Class;

   function Get_Instance (
      District : access Reactive.District.Object'Class := null)
   return Bikeway.Utils.Reference;

   not overriding
   procedure Set_Street (This       : in Bikeway.Utils.Object;
                         Bikeway_Id : in Infra_Id;
                         Street_Id  : in Infra_Id);

   not overriding
   procedure Find_Lane_By_Direction (
      This             : in Bikeway.Utils.Object;
      Bikeway_Id       : in Infra_Id;
      Travel_Direction : in Direction.Straight;
      Lane_Id          : out Infra_Id;
      Found            : out Boolean);

   not overriding
   procedure Validate (This       : in Bikeway.Utils.Object;
                       Bikeway_Id : in Infra_Id);

   function Dump (This : Bikeway.Utils.Object; Bikeway_Id : in Infra_Id)
   return G_JSON.JSON_Value;

private
   type Object is tagged limited record
      District : access Reactive.District.Object'Class;
   end record;

   Instance : Bikeway.Utils.Reference := null;

end Reactive.Infrastructure.Way.Bikeway.Utils;
