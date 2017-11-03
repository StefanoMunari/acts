with Interface_Layer.Service.Activators;

package body Interface_Layer.Service is

   procedure Init (
      Request_Pool_Size : in Stacks.Stack_Range;
      Ack_Pool_Size     : in Stacks.Stack_Range) is
   begin
      Activators.Init (Request_Pool_Size, Ack_Pool_Size);
   end Init;

   procedure Start is
   begin
      Activators.Start;
   end Start;

end Interface_Layer.Service;
