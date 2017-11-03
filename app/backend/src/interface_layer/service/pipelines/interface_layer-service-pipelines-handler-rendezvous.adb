-- core
with Ada.Containers;

-- local
with Interface_Layer.Containers.Queues;
with Interface_Layer.Presentation.Envelope;
with Interface_Layer.Tables.Pending_Request;

with Scheduling.Timing.Callbacks;

with Shared.Indefinite_String_Map;
with Shared.Callback_Pair;

package body Interface_Layer.Service.Pipelines.Handler.Rendezvous is
   use Req_Wrapper; -- view "=" operator
   use Interface_Layer.Containers.Queues; -- view queues instances
   use Ada.Containers;  -- view "=" operator

   package Envelope   renames Interface_Layer.Presentation.Envelope;
   package String_Map renames Shared.Indefinite_String_Map;
   package Pending_Request_Pkg
      renames Interface_Layer.Tables.Pending_Request;
   package Timing_Callbacks_Pkg renames Scheduling.Timing.Callbacks;
   package Callback_Pair_Pkg
      renames Shared.Callback_Pair;

   function Create (
      Next : Handler.Reference;
      Req  : Req_Wrapper.Object := Empty_Request)
   return Rendezvous.Reference
   is
      Instance : Rendezvous.Reference;
   begin
      Instance := new Rendezvous.Object;
      Initialize (Instance, Next, Req);
      return Instance;
   end Create;

   procedure Handle (This : Rendezvous.Object) is
   begin
      if Has_Request (Handler.Object (This))
      then
         declare
            Env : Envelope.Reference := Envelope.Create;
         begin
            This.Handle (Env);
         end;
      end if;
      Handler.Object (This).Handle;
   end Handle;

   procedure Handle (
      This : Rendezvous.Object;
      Env : Envelope.Reference)
   is
   begin
      if Has_Request (Handler.Object (This))
      then
         declare
            Wrapper        : Req_Wrapper.Object :=
               Get_Request (Handler.Object (This));
            Info           : Callback_Pair_Pkg.Object;
            Correlation_Id : SU.Unbounded_String;
         begin
            Wrapper.Get_Request (Info);
            Wrapper.Get_Request (Correlation_Id);
         -- Add the rendezvous object to the set of pending requests
            Pending_Request_Pkg.Table.Add (
               SU.To_String (Correlation_Id), Info);
            Timing_Callbacks_Pkg.Register (Correlation_Id);
         end;
      end if;
      Handler.Object (This).Handle (Env);
   end Handle;

-- private
   procedure Initialize (
      This_Reference : Rendezvous.Reference;
      Next           : Handler.Reference;
      Req            : Req_Wrapper.Object) is
   begin
      Initialize (Handler.Object (This_Reference.all), Next, Req);
   end Initialize;

end Interface_Layer.Service.Pipelines.Handler.Rendezvous;
