-- local
with Interface_Layer.Containers.Queues;
with Interface_Layer.Presentation.Envelope;
with Interface_Layer.Service.Pipelines.Barrier;
with Interface_Layer.Utils.Types;
-- core
with Ada.Containers;

package body Interface_Layer.Service.Pipelines.Handler.Boot is
   use Req_Wrapper; -- view "=" operator
   use Interface_Layer.Containers.Queues; -- view queues instances
   use Ada.Containers;  -- view "=" operator

   package Barrier_Pkg renames Interface_Layer.Service.Pipelines.Barrier;
   package Envelope    renames Interface_Layer.Presentation.Envelope;
   package Types       renames Interface_Layer.Utils.Types;

   function Create return Boot.Reference is
      Instance : Boot.Reference;
   begin
      Instance := new Boot.Object;
      Initialize (Instance, null);
      return Instance;
   end Create;

   procedure Handle (This : Boot.Object) is
   begin
      Barrier_Pkg.Object.Open_Barrier;
   end Handle;

   procedure Handle (This : Boot.Object; Env : Envelope.Reference) is
   begin
      Env.Debug;
      Encoder_Envelope_Queue.Enqueue (Env);
      Barrier_Pkg.Object.Open_Barrier;
   end Handle;

-- private
   procedure Initialize (
      This_Reference :    Boot.Reference;
      Next           : in Interface_Layer.Service.Pipelines.Handler.Reference;
      Req            : in Req_Wrapper.Object := Empty_Request)
   is
   begin
      Initialize (
         Interface_Layer.Service.Pipelines.Handler.Object (This_Reference.all),
         Next,
         Req
      );
   end Initialize;

end Interface_Layer.Service.Pipelines.Handler.Boot;
