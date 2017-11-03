with Active.Agent;

with Reactive.Infrastructure.Stretch.Decoration.Stretch_Decorator;

with Shared.Agent_Id_List;

package Reactive.Infrastructure.Stretch.Decoration.Zebra_Crossing is

   package Agent renames Active.Agent;

   type Object is abstract
      new Stretch_Decorator.Object
   with private;
   type Reference is access all Object'Class;

   not overriding
   procedure Init (This : Zebra_Crossing.Reference);

   overriding
   procedure Tread (This         : in out Zebra_Crossing.Object;
                    Traveller_Id : in Agent.Agent_Id;
                    Advanced     : out Boolean);

   overriding
   procedure Leave (
      This         : in out Zebra_Crossing.Object;
      Traveller_Id : in     Agent.Agent_Id;
      Left         :    out Boolean);

   not overriding
   function Has_Priority_Over (
      This         : in out Zebra_Crossing.Object;
      Traveller_Id : in     Agent.Agent_Id) return Boolean is abstract;

private

   protected type Priority_Enforcement is

      procedure Enqueue_Waiting_Privileged (Traveller_Id : Agent.Agent_Id);

      procedure Dequeue_Waiting_Privileged (Traveller_Id : Agent.Agent_Id);

      function Has_Waiting_Privileged return Boolean;

      procedure Enqueue_Waiting_Unprivileged (Traveller_Id : Agent.Agent_Id);

      procedure Dequeue_Waiting_Unprivileged (Traveller_Id : Agent.Agent_Id);

      function Has_Waiting_Unprivileged return Boolean;

      procedure Enqueue_Treading_Privileged (Traveller_Id : Agent.Agent_Id);

      procedure Dequeue_Treading_Privileged (Traveller_Id : Agent.Agent_Id);

      function Has_Treading_Privileged return Boolean;

      procedure Enqueue_Treading_Unprivileged (Traveller_Id : Agent.Agent_Id);

      procedure Dequeue_Treading_Unprivileged (Traveller_Id : Agent.Agent_Id);

      function Has_Treading_Unprivileged return Boolean;

      procedure Get_In_Privileged (Traveller_Id : in     Agent.Agent_Id;
                                   Got_In       :    out Boolean);

      procedure Get_In_Unprivileged (Traveller_Id : in     Agent.Agent_Id;
                                     Got_In       :    out Boolean);

   private
      Waiting_Privileged_Travellers    : Agent_Id_List.List;
      Waiting_Unprivileged_Travellers  : Agent_Id_List.List;
      Treading_Privileged_Travellers   : Agent_Id_List.List;
      Treading_Unprivileged_Travellers : Agent_Id_List.List;
   end Priority_Enforcement;

   type Object is abstract
      new Stretch_Decorator.Object
   with record
      Priority_Enforcement : access Zebra_Crossing.Priority_Enforcement;
   end record;

   overriding
   function Get_Travellers_Queue (This : in Zebra_Crossing.Object)
   return access Stretch.Protected_Travellers_Queue;

end Reactive.Infrastructure.Stretch.Decoration.Zebra_Crossing;
