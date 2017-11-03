-- core
with Ada.Containers;

-- local
with Interface_Layer.Containers.Queues;
with Interface_Layer.Presentation.Envelope;
with Interface_Layer.Utils.Types;

package body Interface_Layer.Service.Pipelines.Handler.Call is
   use Req_Wrapper; -- view "=" operator
   use Interface_Layer.Containers.Queues; -- view queues instances
   use Ada.Containers;  -- view "=" operator

   package Envelope renames Interface_Layer.Presentation.Envelope;
   package Types    renames Interface_Layer.Utils.Types;

   function Create (
      Next : Handler.Reference;
      Req  : Req_Wrapper.Object := Empty_Request)
   return Call.Reference
   is
      Instance : Call.Reference;
   begin
      Instance := new Call.Object;
      Initialize (Instance, Next, Req);
      return Instance;
   end Create;

   procedure Handle (This : Call.Object) is
   begin
      if Has_Request (Interface_Layer.Service.Pipelines.Handler.Object (This))
      then
         declare
            Env : Envelope.Reference := Envelope.Create;
         begin
            This.Handle (Env);
         end;
      else
        Interface_Layer.Service.Pipelines.Handler.Object (This).Handle;
      end if;
   end Handle;

   procedure Handle (This : Call.Object; Env : Envelope.Reference) is
   begin
      if Has_Request (Interface_Layer.Service.Pipelines.Handler.Object (This))
      then
         declare
            Wrapper : Req_Wrapper.Object := Get_Request (Handler.Object (This));
            Info    : Types.Call_Type;
         begin
            Wrapper.Get_Request (Info);
            Env.all.Set_Header (Info);
         -- Encoder_Envelope_Queue.Enqueue (Env);
         end;
      end if;
      Handler.Object (This).Handle (Env);
   end Handle;

-- private
   procedure Initialize (
      This_Reference :    Call.Reference;
      Next           : in Handler.Reference;
      Req            : in Req_Wrapper.Object) is
   begin
      Initialize (Handler.Object (This_Reference.all), Next, Req);
   end Initialize;

end Interface_Layer.Service.Pipelines.Handler.Call;
