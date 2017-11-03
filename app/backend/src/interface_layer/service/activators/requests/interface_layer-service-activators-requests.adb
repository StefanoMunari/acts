-- local
with Interface_Layer.Service.Activators.Requests.Executors;
-- core
with Ada.Unchecked_Deallocation;
-- DEBUG
-- DEBUG

package body Interface_Layer.Service.Activators.Requests is

   -- TODO: implement scoped_pointers
   procedure Free is new Ada.Unchecked_Deallocation (Request, Reference);

   procedure Init (Pool_Size : Stacks.Stack_Range) is separate;
   procedure Start is separate;
   procedure Shutdown is separate;

-- private

   function Is_Shutdown (Action : String) return Boolean is separate;

   task body Request is
      use PT; -- make '=' visible for Process_Types
      use Interface_Layer.Service.Activators.Requests.Executors;
   begin
      select
         accept Active;
         declare
            Request_Workers :
               array (1.. Req_Stack_Instance.Get_Size) of Executor;
            Req_Task_Index : Stacks.Stack_Range;
         begin
            while Request_State = PT.ACTIVE loop
            -- when ready execute, otherwise terminate
               if not Req_Stack_Instance.Is_Empty then
               -- Get Index from Stack_Instance (PO)
                  Req_Stack_Instance.Pop (Req_Task_Index);
               -- assign the Index to the task
                  Request_Workers (Req_Task_Index).Init (Req_Task_Index);
               -- execute the ASYNC task
                  Request_Workers (Req_Task_Index).Exec;
               end if;
            end loop;

         -- when no more active, terminate
            --for Worker of Request_Workers loop
            --   Worker.Shutdown;
            --end loop;
            Requests.Shutdown;
         end;
      or
        terminate;
      end select;
   end Request;

   -- begin
   -- -- free resources
   -- Free (Request_Ref);
end Interface_Layer.Service.Activators.Requests;
