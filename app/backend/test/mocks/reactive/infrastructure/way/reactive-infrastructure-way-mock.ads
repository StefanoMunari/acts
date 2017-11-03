with Active.Agent;

package Reactive.Infrastructure.Way.Mock is

   package Agent renames Active.Agent;

   type Object is new Way.Object with private;
   type Reference is access all Way.Mock.Object'Class;

   function Create return Way.Mock.Reference;

   overriding
   function Find_Street (This : in Way.Mock.Object)
                         return Infra_Id;

   overriding
   function Get_Id (This : in Way.Mock.Object) return Infra_Id;

   overriding
   function Is_Contained_By (This         : in Way.Mock.Object;
                             Container_Id : in Infra_Id) return Boolean;

   overriding
   procedure Add_Lane (This    : in out Way.Mock.Object;
                       Lane_Id : in Infra_Id;
                       Added   : out Boolean);

   overriding
   procedure Find_Lane_By_Direction (
      This             : in Way.Mock.Object;
      Travel_Direction : in Direction.Straight;
      Lane_Id          : out Infra_Id;
      Found            : out Boolean);

   overriding
   procedure Set_Street (This      : out Way.Mock.Object;
                         Street_Id : in Infra_Id) is null;

   not overriding
   procedure Set_Return_Value_For_Find_Street (
      This         : in out Way.Mock.Object;
      Return_Value : in Infra_Id);

   not overriding
   procedure Set_Id (
      This         : in out Way.Mock.Object;
      Return_Value : in Infra_Id);

   not overriding
   procedure Set_Return_Value_For_Is_Contained_By (
      This         : in out Way.Mock.Object;
      Return_Value : in Boolean);

   not overriding
   procedure Set_Return_Value_For_Add_Lane (
      This  : in out Way.Mock.Object;
      Added : in Boolean);

   not overriding
   procedure Set_Return_Value_For_Find_Lane_By_Direction (
      This    : in out Way.Mock.Object;
      Lane_Id : in Infra_Id;
      Found   : in Boolean);

private
   type Mock_Values_Collection is record
      Id : Infra_Id;
      Id_Existence : Boolean := FALSE;
   end record;

   type Return_Values_Collection is record
      Find_Street : Infra_Id;
      Find_Street_Existence : Boolean := FALSE;
      Is_Contained_By : Boolean;
      Is_Contained_By_Existence : Boolean := FALSE;
      Add_Lane : Boolean;
      Add_Lane_Existence : Boolean := FALSE;
      Find_Lane_By_Direction : Infra_Id;
      Find_Lane_By_Direction_Found : Boolean;
      Find_Lane_By_Direction_Existence : Boolean := FALSE;
   end record;

   type Object is
     new Way.Object with record
      Mock_Values : Mock_Values_Collection;
      Return_Values : Return_Values_Collection;
   end record;

end Reactive.Infrastructure.Way.Mock;
