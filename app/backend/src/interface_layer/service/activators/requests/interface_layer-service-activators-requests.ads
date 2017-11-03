-- local
with Interface_Layer.Containers.Stacks;

with Shared.Process_Types;

package Interface_Layer.Service.Activators.Requests is
   package Stacks renames Interface_Layer.Containers.Stacks;
   package PT     renames Shared.Process_Types;

   procedure Init (Pool_Size : Stacks.Stack_Range);
   procedure Start;
   procedure Shutdown;

private

   function Is_Shutdown (Action : String) return Boolean;

   task type Request is
      entry Active;
   end Request;
   type Reference is access Request;

   -- static data fields
   Req_Stack_Instance : Stacks.Stack;
   Request_State    : PT.Process_T := PT.TERMINATED;
   Request_Ref      : Requests.Reference := NULL;

end Interface_Layer.Service.Activators.Requests;
