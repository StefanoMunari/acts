package Active.Traveller.Strategy.Mock is

   package Agent renames Active.Agent;

   type Object is new Strategy.Object with private;
   type Reference is access all Strategy.Mock.Object'Class;

   function Create
   return Strategy.Mock.Reference;

   overriding
   function Wait_For_Bus_Or_Not (
      This           : Strategy.Mock.Object;
      Pedestrian_Id  : Agent.Agent_Id;
      Current_Stretch : Infra_Id;
      Bus_Stop_Ref   : Road_Sign.Bus_Stop.Reference)
   return Boolean;

   not overriding
   procedure Set_Return_Value_For_Wait_For_Bus_Or_Not (
      This         : in out Strategy.Mock.Object;
      Return_Value : in     Boolean);

private
   type Return_Values_Collection is record
      Wait_For_Bus_Or_Not : Boolean;
      Wait_For_Bus_Or_Not_Existence : Boolean := FALSE;
   end record;

   type Object is new Strategy.Object
   with record
      Return_Values : Return_Values_Collection;
   end record;

end Active.Traveller.Strategy.Mock;
