-- local
with Interface_Layer.Service.Activators.Requests;
with Interface_Layer.Service.Activators.Acks;
-- DEBUG
with Ada.Text_IO;
-- DEBUG

package body Interface_Layer.Service.Activators is

   package Requests_Pkg renames Interface_Layer.Service.Activators.Requests;
   package Acks_Pkg renames Interface_Layer.Service.Activators.Acks;

   procedure Init (Req_Pool_Size : Stacks.Stack_Range;
                   Ack_Pool_Size : Stacks.Stack_Range) is separate;
   procedure Start is separate;
   procedure Shutdown is separate;

end Interface_Layer.Service.Activators;
