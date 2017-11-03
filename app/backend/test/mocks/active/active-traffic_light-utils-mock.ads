limited with Reactive.District.Mock;

with Shared.Agent_Boolean_Map;

package Active.Traffic_Light.Utils.Mock is

   package Agent renames Active.Agent;
   package Agent_Boolean_Map renames Shared.Agent_Boolean_Map;

   type Object (<>) is new Traffic_Light.Utils.Object with private;
   type Reference is access all Traffic_Light.Utils.Mock.Object'Class;

   function Create return Traffic_Light.Utils.Mock.Reference;

   overriding
   function Is_A_Traffic_Light (This      : in out Mock.Object;
                                Active_Id : in     Agent.Agent_Id)
   return Boolean;

   not overriding
   procedure Set_Value_For_Is_A_Traffic_Light (
      This               : in out Mock.Object;
      Active_Id          : in     Agent.Agent_Id;
      Is_A_Traffic_Light : in     Boolean);

private
   type Return_Values_Collection is record
      Is_A_Traffic_Light : Agent_Boolean_Map.Map
        := Agent_Boolean_Map.Empty_Map;
      Is_A_Traffic_Light_Existence : Boolean := False;
   end record;

   type Object is new Traffic_Light.Utils.Object with record
      Return_Values : Return_Values_Collection;
   end record;

end Active.Traffic_Light.Utils.Mock;
