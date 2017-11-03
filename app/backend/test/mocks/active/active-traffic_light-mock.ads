with Shared.Boolean_Map;

package Active.Traffic_Light.Mock is

   package Boolean_Map renames Shared.Boolean_Map;

   type Object is new Traffic_Light.Object with private;
   type Reference is access all Traffic_Light.Mock.Object'Class;

   not overriding
   function New_Mock return Traffic_Light.Mock.Reference;

   overriding
   function Get_Id (This : in Traffic_Light.Mock.Object) return Agent.Agent_Id;

   overriding
   procedure Act (This : in out Traffic_Light.Mock.Object) is null;

   --overriding
   --function "=" (This, Other : Traffic_Light.Mock.Object) return Boolean;

   not overriding
   procedure Set_Id (
      This  : in out Traffic_Light.Mock.Object;
      Id    : in     Agent.Agent_Id);

private
   type Mock_Values_Collection is record
      Id : Agent.Agent_Id;
      Id_Existence : Boolean := False;
   end record;

   type Object is
     new Traffic_Light.Object
   with record
      Mock_Values : Mock_Values_Collection;
   end record;

end Active.Traffic_Light.Mock;
