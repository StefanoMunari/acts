-- local
with Interface_Layer.Tables.Dispatcher;
with Interface_Layer.Containers.Stacks;

package Interface_Layer.Service.Activators is
   package Interface_Dispatcher renames Interface_Layer.Tables.Dispatcher;
   package Stacks               renames Interface_Layer.Containers.Stacks;

   procedure Init (Req_Pool_Size : Stacks.Stack_Range;
                   Ack_Pool_Size : Stacks.Stack_Range);
   procedure Start;
   procedure Shutdown;

private

   -- static data fields
   Dispatcher : Interface_Dispatcher.Object;

end Interface_Layer.Service.Activators;
