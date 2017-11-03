with Active.Agent;

with Shared.Infra_Id_To_Boolean_Map;

package Reactive.Infrastructure.Way.Roadway.Mock is

   package Agent renames Active.Agent;
   package Infra_Id_To_Boolean_Map renames Shared.Infra_Id_To_Boolean_Map;

   type Object is new Roadway.Object with private;
   type Reference is access all Roadway.Mock.Object'Class;

   function Create return Roadway.Mock.Reference;

   overriding
   function Find_Street (This : in Roadway.Mock.Object)
   return Infra_Id;

   overriding
   function Get_Id (This : in Roadway.Mock.Object) return Infra_Id;

   overriding
   function Is_Contained_By (This         : in Roadway.Mock.Object;
                             Container_Id : in Infra_Id)
   return Boolean;

   overriding
   procedure Add_Lane (This    : in out Roadway.Mock.Object;
                       Lane_Id : in     Infra_Id;
                       Added   :    out Boolean);

   overriding
   procedure Find_Lane_By_Direction (
      This             : in     Roadway.Mock.Object;
      Travel_Direction : in     Direction.Straight;
      Lane_Id          :    out Infra_Id;
      Found            :    out Boolean);

   overriding
   procedure Set_Street (This      :    out Roadway.Mock.Object;
                         Street_Id : in     Infra_Id) is null;

   not overriding
   procedure Set_Return_Value_For_Find_Street (
      This         : in out Roadway.Mock.Object;
      Return_Value : in     Infra_Id);

   not overriding
   procedure Set_Id (
      This         : in out Roadway.Mock.Object;
      Return_Value : in     Infra_Id);

   not overriding
   procedure Set_Return_Value_For_Is_Contained_By (
      This         : in out Roadway.Mock.Object;
      Container_Id : in     Infra_Id;
      Return_Value : in     Boolean);

   not overriding
   procedure Set_Return_Value_For_Add_Lane (
      This  : in out Roadway.Mock.Object;
      Added : in     Boolean);

   not overriding
   procedure Set_Return_Value_For_Find_Lane_By_Direction (
      This    : in out Roadway.Mock.Object;
      Lane_Id : in    Infra_Id;
      Found   : in    Boolean);

private
   type Mock_Values_Collection is record
      Id : Infra_Id;
      Id_Existence : Boolean := FALSE;
   end record;

   type Return_Values_Collection is record
      Find_Street : Infra_Id;
      Find_Street_Existence : Boolean := FALSE;
      Is_Contained_By : Infra_Id_To_Boolean_Map.Map;
      Is_Contained_By_Existence : Boolean := FALSE;
      Add_Lane : Boolean;
      Add_Lane_Existence : Boolean := FALSE;
      Find_Lane_By_Direction : Infra_Id;
      Find_Lane_By_Direction_Found : Boolean;
      Find_Lane_By_Direction_Existence : Boolean := FALSE;
   end record;

   type Object is
     new Roadway.Object with record
      Mock_Values : Mock_Values_Collection;
      Return_Values : Return_Values_Collection;
   end record;

end Reactive.Infrastructure.Way.Roadway.Mock;
