-- local
with Interface_Layer.Containers.Stacks;

with Shared.Process_Types;

package Interface_Layer.Service.Activators.Acks is
   package Stacks renames Interface_Layer.Containers.Stacks;
   package PT     renames Shared.Process_Types;

   procedure Init (Pool_Size : Stacks.Stack_Range);
   procedure Start;
   procedure Stop;
   procedure Shutdown;

private

   function Is_Shutdown (Action : String) return Boolean;

   task type Ack is
      entry Active;
   end Ack;
   type Reference is access Ack;

   -- static data fields
   Ack_Stack_Instance : Stacks.Stack;
   Ack_State    : PT.Process_T := PT.TERMINATED;
   Ack_Ref      : Acks.Reference := NULL;

end Interface_Layer.Service.Activators.Acks;
