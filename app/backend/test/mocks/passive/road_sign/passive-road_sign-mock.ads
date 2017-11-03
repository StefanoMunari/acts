with Active.Agent;

with Shared.Agent_Id_Set;

package Passive.Road_Sign.Mock is

   package Agent renames Active.Agent;
   package Agent_Id_Set renames Shared.Agent_Id_Set;

   type Object (<>) is new Passive.Road_Sign.Object with private;
   type Reference is access all Road_Sign.Mock.Object'Class;

   function Create return Road_Sign.Reference;

   overriding
   procedure Apply (This      : in out Road_Sign.Mock.Object;
                    Traveller : in     Agent.Agent_Id);

   overriding
   function Dump (This : Road_Sign.Mock.Object)
   return G_JSON.JSON_Value;

   not overriding
   function Get_Apply_For (This      : in Road_Sign.Mock.Object;
                           Traveller : in Agent.Agent_Id)
   return Boolean;

   not overriding
   procedure Set_Return_Value_For_Dump (
      This         : in out Road_Sign.Mock.Object;
      Return_Value : in     G_JSON.JSON_Value);

private
   type Return_Values_Collection is record
      Dump : G_JSON.JSON_Value;
      Dump_Existence : Boolean := FALSE;
   end record;

   type Object is
     new Passive.Road_Sign.Object
   with record
      Applied_For : Agent_Id_Set.Set;
      Return_Values : Return_Values_Collection;
   end record;

end Passive.Road_Sign.Mock;
