with Shared.Agent_Boolean_Map;
with Shared.Agent_Id_Set;

package Active.Traveller.Pedestrian.Utils.Mock is

   package Agent_Boolean_Map renames Shared.Agent_Boolean_Map;
   package Agent_Id_Set renames Shared.Agent_Id_Set;

   type Object (<>) is new Pedestrian.Utils.Object with private;
   type Reference is access all Pedestrian.Utils.Mock.Object'Class;

   function Create
   return Pedestrian.Utils.Mock.Reference;

   overriding
   function Is_A_Pedestrian (
      This         : in Pedestrian.Utils.Mock.Object;
      Traveller_Id : in Agent.Agent_Id)
   return Boolean;

   overriding
   procedure Stop_Waiting (
      This         : in out Pedestrian.Utils.Mock.Object;
      Traveller_Id : in     Agent.Agent_Id);

   overriding
   procedure Recompute_Travel (
      This          : Pedestrian.Utils.Mock.Object;
      Traveller_Id  : Agent.Agent_Id) is null;

   procedure Set_Return_Value_For_Is_A_Pedestrian (
      This         : in out Pedestrian.Utils.Mock.Object;
      Traveller_Id : in     Agent.Agent_Id;
      Return_Value : in     Boolean);

   function Get_Stop_Waiting_For_Id (
      This         : in out Pedestrian.Utils.Mock.Object;
      Traveller_Id : in     Agent.Agent_Id)
   return Boolean;

private
   type Return_Values_Collection is record
      Is_A_Pedestrian : Agent_Boolean_Map.Map;
   end record;

   type Mock_Values_Collection is record
      Stop_Waiting : Agent_Id_Set.Set;
   end record;

   type Object is new Pedestrian.Utils.Object with record
      Return_Values : Return_Values_Collection;
      Mock_Values   : Mock_Values_Collection;
   end record;

end Active.Traveller.Pedestrian.Utils.Mock;
