with Interface_Layer.Containers.Stacks;

package Interface_Layer.Service is

   package Stacks renames Interface_Layer.Containers.Stacks;

   procedure Init (
      Request_Pool_Size : in Stacks.Stack_Range;
      Ack_Pool_Size     : in Stacks.Stack_Range);
   procedure Start;

end Interface_Layer.Service;
