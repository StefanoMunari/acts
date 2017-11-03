package Active.Agent.Mock is

   type Object (<>) is new Active.Agent.Object with private;
   type Reference is access all Agent.Mock.Object'Class;

   function Create return Agent.Reference;

   overriding
   function Get_Id (This : Agent.Mock.Object) return Agent_Id;

   overriding
   procedure Act (This : in out Active.Agent.Mock.Object);

   not overriding
   procedure Set_Id (This : in out Agent.Mock.Object; Id : in Agent_Id);

   not overriding
   function Get_Act_Called (This : Agent.Mock.Object) return Boolean;

private

   protected type Rendezvous_Object is
      entry Wait;
      entry Notify;
   private
      Act_Called : Boolean := FALSE;
   end Rendezvous_Object;
   type Rendezvous_Reference is access Rendezvous_Object;

   type Return_Values_Collection is record
      Id : Agent_Id;
      Id_Existence : Boolean := False;
   end record;

   type Object is
     new Active.Agent.Object
   with record
      Rendezvous : Rendezvous_Reference := new Rendezvous_Object;
      Return_Values : Return_Values_Collection;
   end record;

end Active.Agent.Mock;
