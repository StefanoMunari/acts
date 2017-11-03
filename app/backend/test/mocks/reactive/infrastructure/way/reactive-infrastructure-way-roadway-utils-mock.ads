with Active.Agent;

package Reactive.Infrastructure.Way.Roadway.Utils.Mock is

   package Agent renames Active.Agent;

   type Object (<>) is new Roadway.Utils.Object with private;
   type Reference is access all Roadway.Utils.Mock.Object'Class;

   function Create return Roadway.Utils.Mock.Reference;

   overriding
   procedure Set_Street (This       : in Roadway.Utils.Mock.Object;
                         Roadway_Id : in Infra_Id;
                         Street_Id  : in Infra_Id);

   overriding
   procedure Find_Lane_By_Direction (
      This             : in     Roadway.Utils.Mock.Object;
      Roadway_Id       : in     Infra_Id;
      Travel_Direction : in     Direction.Straight;
      Lane_Id          :    out Infra_Id;
      Found            :    out Boolean);

   overriding
   procedure Validate (This       : in Roadway.Utils.Mock.Object;
                       Roadway_Id : in Infra_Id);

   not overriding
   procedure Set_Return_Value_For_Find_Lane_By_Direction (
      This    : in out Roadway.Utils.Mock.Object;
      Lane_Id : in    Infra_Id;
      Found   : in    Boolean);

private
   type Return_Values_Collection is record
      Find_Lane_By_Direction : Infra_Id;
      Find_Lane_By_Direction_Found : Boolean;
      Find_Lane_By_Direction_Existence : Boolean := FALSE;
   end record;

   type Object is new Roadway.Utils.Object with record
      Return_Values : Return_Values_Collection;
   end record;

end Reactive.Infrastructure.Way.Roadway.Utils.Mock;
