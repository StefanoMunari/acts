with Active.Agent;

package Reactive.Infrastructure.Way.Footway.Utils.Mock is

   package Agent renames Active.Agent;

   type Object (<>) is new Footway.Utils.Object with private;
   type Reference is access all Footway.Utils.Mock.Object'Class;

   function Create return Footway.Utils.Mock.Reference;

   overriding
   procedure Set_Street (This       : in Footway.Utils.Mock.Object;
                         Footway_Id : in Infra_Id;
                         Street_Id  : in Infra_Id);

   overriding
   procedure Find_Lane_By_Direction (
      This             : in     Footway.Utils.Mock.Object;
      Footway_Id       : in     Infra_Id;
      Travel_Direction : in     Direction.Straight;
      Lane_Id          :    out Infra_Id;
      Found            :    out Boolean);

   overriding
   procedure Validate (This       : in Footway.Utils.Mock.Object;
                       Footway_Id : in Infra_Id);

   not overriding
   procedure Set_Return_Value_For_Find_Lane_By_Direction (
      This    : in out Footway.Utils.Mock.Object;
      Lane_Id : in     Infra_Id;
      Found   : in     Boolean);

private
   type Return_Values_Collection is record
      Find_Lane_By_Direction : Infra_Id;
      Find_Lane_By_Direction_Found : Boolean;
      Find_Lane_By_Direction_Existence : Boolean := FALSE;
   end record;

   type Object is new Footway.Utils.Object with record
      Return_Values : Return_Values_Collection;
   end record;

end Reactive.Infrastructure.Way.Footway.Utils.Mock;
