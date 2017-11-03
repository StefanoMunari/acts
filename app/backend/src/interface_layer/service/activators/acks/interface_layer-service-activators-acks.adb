-- local
with Interface_Layer.Service.Activators.Acks.Executors;
-- core
with Ada.Unchecked_Deallocation;

package body Interface_Layer.Service.Activators.Acks is

   -- TODO: implement scoped_pointers
   procedure Free is new Ada.Unchecked_Deallocation (Ack, Reference);

   procedure Init (Pool_Size : Stacks.Stack_Range) is separate;
   procedure Start is separate;
   procedure Stop is separate;
   procedure Shutdown is separate;

-- private

   function Is_Shutdown (Action : String) return Boolean is separate;

   task body Ack is
      use PT; -- make '=' visible for Process_Types
      use Interface_Layer.Service.Activators.Acks.Executors;
   begin
      select
         accept Active;
         declare
            Ack_Workers :
               array (1.. Ack_Stack_Instance.Get_Size) of Executor;
            Ack_Task_Index : Stacks.Stack_Range;
         begin
            while Ack_State = PT.ACTIVE loop
            -- when ready execute, otherwise terminate
               if not Ack_Stack_Instance.Is_Empty then
               -- Get Index from Stack_Instance (PO)
                  Ack_Stack_Instance.Pop (Ack_Task_Index);
               -- assign the Index to the task in RENDEZVOUS
                  Ack_Workers (Ack_Task_Index).Init (Ack_Task_Index);
               -- execute the ASYNC task
                  Ack_Workers (Ack_Task_Index).Exec;
               end if;
            end loop;

         -- when no more active, terminate
            --for Worker of Ack_Workers loop
            --   Worker.Shutdown;
            --end loop;
            Acks.Shutdown;
         end;
      or
        terminate;
      end select;
   end Ack;

   -- begin
   -- -- free resources
   -- Free (Ack_Ref);

end Interface_Layer.Service.Activators.Acks;
