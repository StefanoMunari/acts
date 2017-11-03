package body Reactive.Infrastructure.Stretch.Decoration.Zebra_Crossing is

   not overriding
   procedure Init (This : Zebra_Crossing.Reference) is
   begin
      This.Priority_Enforcement := new Zebra_Crossing.Priority_Enforcement;
   end Init;

   procedure Tread (This         : in out Zebra_Crossing.Object;
                    Traveller_Id : in Agent.Agent_Id;
                    Advanced     : out Boolean) is
      This_Ref         : Zebra_Crossing.Object'Class := This;
      Has_Priority     : Boolean;
      Got_In           : Boolean;
      Travellers_Queue : access Stretch.Protected_Travellers_Queue
         := This.Get_Travellers_Queue;
   begin
      Has_Priority := This_Ref.Has_Priority_Over (Traveller_Id);
      if Has_Priority then
         This.Priority_Enforcement.Enqueue_Waiting_Privileged (Traveller_Id);
         This.Priority_Enforcement.Get_In_Privileged   (Traveller_Id, Got_In);
      else
         This.Priority_Enforcement.Get_In_Unprivileged (Traveller_Id, Got_In);
      end if;
      if not Got_In then
         Advanced := False;
         This.Priority_Enforcement.Dequeue_Waiting_Unprivileged (
            Traveller_Id);
         return;
      end if;

      if Has_Priority  then
         Travellers_Queue.Enter_Into_Stretch (Traveller_Id, Advanced);
      else
         This.Get_Stretch_Ref.Tread (Traveller_Id, Advanced);
      end if;

      if not Has_Priority then
         if Advanced then
            This.Priority_Enforcement.Enqueue_Treading_Unprivileged (
               Traveller_Id);
         end if;
   -- Dequeue unprivileged from waiting list regardless of successful stretch
   --+treading
         This.Priority_Enforcement.Dequeue_Waiting_Unprivileged (
            Traveller_Id);
      else
      -- branch for privileged travellers
         if Advanced then
         -- order matters!
            This.Priority_Enforcement.Enqueue_Treading_Privileged (
               Traveller_Id);
            This.Priority_Enforcement.Dequeue_Waiting_Privileged (
               Traveller_Id);
         end if;
      end if;
   end Tread;

   procedure Leave (
      This         : in out Zebra_Crossing.Object;
      Traveller_Id : in     Agent.Agent_Id;
      Left         :    out Boolean)
   is
      This_Ref     : Zebra_Crossing.Object'Class := This;
      Has_Priority : Boolean;
   begin
      Has_Priority := This_Ref.Has_Priority_Over (Traveller_Id);
      This.Get_Stretch_Ref.Leave (Traveller_Id, Left);
      if not Left then
         return;
      end if;
      if Has_Priority then
         This.Priority_Enforcement.Dequeue_Treading_Privileged
            (Traveller_Id);
      else
         This.Priority_Enforcement.Dequeue_Treading_Unprivileged
            (Traveller_Id);
      end if;
   end Leave;

   function Get_Travellers_Queue (This : in Zebra_Crossing.Object)
   return access Stretch.Protected_Travellers_Queue
   is (This.Get_Stretch_Ref.Get_Travellers_Queue);

   ----------------------------
   --- PRIORITY ENFORCEMENT
   ----------------------------

-- Most of the following code is basically a wrapper for atomic insertion and
--+ deletion from several lists.
   protected body Priority_Enforcement is

   -- WAITING PRIVILEGED
      procedure Enqueue_Waiting_Privileged (Traveller_Id : Agent.Agent_Id) is
      begin
         if Waiting_Privileged_Travellers.Contains (Traveller_Id) then
            return;
         end if;
         Waiting_Privileged_Travellers.Append (Traveller_Id);
      end Enqueue_Waiting_Privileged;

      procedure Dequeue_Waiting_Privileged (Traveller_Id : Agent.Agent_Id) is
         Traveller_Cursor : Agent_Id_List.Cursor;
      begin
         Traveller_Cursor := Waiting_Privileged_Travellers.Find (Traveller_Id);
         if Agent_Id_List.Has_Element (Traveller_Cursor) then
            Waiting_Privileged_Travellers.Delete (Traveller_Cursor);
         end if;
      end Dequeue_Waiting_Privileged;

      function Has_Waiting_Privileged return Boolean is
      begin
         return Natural (Waiting_Privileged_Travellers.Length) > 0;
      end Has_Waiting_Privileged;

   -- WAITING UNPRIVILEGED
      procedure Enqueue_Waiting_Unprivileged (Traveller_Id : Agent.Agent_Id) is
      begin
         if Waiting_Unprivileged_Travellers.Contains (Traveller_Id) then
            return;
         end if;
         Waiting_Unprivileged_Travellers.Append (Traveller_Id);
      end Enqueue_Waiting_Unprivileged;

      procedure Dequeue_Waiting_Unprivileged (Traveller_Id : Agent.Agent_Id)
      is
         Traveller_Cursor : Agent_Id_List.Cursor;
      begin
         Traveller_Cursor :=
            Waiting_Unprivileged_Travellers.Find (Traveller_Id);
         if Agent_Id_List.Has_Element (Traveller_Cursor) then
            Waiting_Unprivileged_Travellers.Delete (Traveller_Cursor);
         end if;
      end Dequeue_Waiting_Unprivileged;

      function Has_Waiting_Unprivileged return Boolean is
      begin
         return Natural (Waiting_Unprivileged_Travellers.Length) > 0;
      end Has_Waiting_Unprivileged;

   -- TREADING PRIVILEGED
      procedure Enqueue_Treading_Privileged (Traveller_Id : Agent.Agent_Id) is
      begin
         if Treading_Privileged_Travellers.Contains (Traveller_Id) then
            return;
         end if;
         Treading_Privileged_Travellers.Append (Traveller_Id);
      end Enqueue_Treading_Privileged;

      procedure Dequeue_Treading_Privileged (Traveller_Id : Agent.Agent_Id)
      is
         Traveller_Cursor : Agent_Id_List.Cursor;
      begin
         Traveller_Cursor :=
            Treading_Privileged_Travellers.Find (Traveller_Id);
         if Agent_Id_List.Has_Element (Traveller_Cursor) then
            Treading_Privileged_Travellers.Delete (Traveller_Cursor);
         end if;
      end Dequeue_Treading_Privileged;

      function Has_Treading_Privileged return Boolean is
      begin
         return Natural (Treading_Privileged_Travellers.Length) > 0;
      end Has_Treading_Privileged;

   -- TREADING UNPRIVILEGED
      procedure Enqueue_Treading_Unprivileged (Traveller_Id : Agent.Agent_Id)
      is
      begin
         if Treading_Unprivileged_Travellers.Contains (Traveller_Id) then
            return;
         end if;
         Treading_Unprivileged_Travellers.Append (Traveller_Id);
      end Enqueue_Treading_Unprivileged;

      procedure Dequeue_Treading_Unprivileged (Traveller_Id : Agent.Agent_Id)
      is
         Traveller_Cursor : Agent_Id_List.Cursor;
      begin
         Traveller_Cursor :=
            Treading_Unprivileged_Travellers.Find (Traveller_Id);
         if Agent_Id_List.Has_Element (Traveller_Cursor) then
            Treading_Unprivileged_Travellers.Delete (Traveller_Cursor);
         end if;
      end Dequeue_Treading_Unprivileged;

      function Has_Treading_Unprivileged return Boolean is
      begin
         return Natural (Treading_Unprivileged_Travellers.Length) > 0;
      end Has_Treading_Unprivileged;

      procedure Get_In_Privileged (Traveller_Id : in     Agent.Agent_Id;
                                   Got_In       :    out Boolean) is
      begin
         if Has_Waiting_Unprivileged or Has_Treading_Unprivileged then
            Got_In := False;
         else
            Got_In := True;
         end if;
      end Get_In_Privileged;

      procedure Get_In_Unprivileged (Traveller_Id : in     Agent.Agent_Id;
                                     Got_In       :    out Boolean) is
      begin
         if Has_Waiting_Privileged or Has_Treading_Privileged then
            Got_In := False;
         else
            Got_In := True;
         end if;
            Enqueue_Waiting_Unprivileged (Traveller_Id);
      end Get_In_Unprivileged;

   end Priority_Enforcement;

end Reactive.Infrastructure.Stretch.Decoration.Zebra_Crossing;
