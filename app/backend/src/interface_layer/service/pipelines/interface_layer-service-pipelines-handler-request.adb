-- core
with Ada.Containers;

-- local
with Interface_Layer.Presentation.Envelope;
with Interface_Layer.Utils.Types;
with Interface_Layer.Containers.Queues;

with Shared.Indefinite_String_Map;

package body Interface_Layer.Service.Pipelines.Handler.Request is
   use Req_Wrapper; -- view "=" operator
   use Interface_Layer.Containers.Queues; -- view queues instances
   use Ada.Containers;  -- view "=" operator

   package Envelope renames Interface_Layer.Presentation.Envelope;
   package Types renames Interface_Layer.Utils.Types;
   package String_Map renames Shared.Indefinite_String_Map;

   function Create (
      Next : Handler.Reference;
      Req  : Req_Wrapper.Object := Empty_Request)
   return Request.Reference
   is
      Instance : Request.Reference;
   begin
      Instance := new Request.Object;
      Initialize (Instance, Next, Req);
      return Instance;
   end Create;

   procedure Handle (This : Request.Object) is
   begin
      if Has_Request (Handler.Object (This))
      then
         declare
            Env : Envelope.Reference := Envelope.Create;
         begin
            This.Handle (Env);
         end;
      else
         Handler.Object (This).Handle;
      end if;
   end Handle;

   procedure Handle (This : Request.Object; Env : Envelope.Reference) is
   begin
      if Has_Request (Interface_Layer.Service.Pipelines.Handler.Object (This))
      then
         declare
            Wrapper : Req_Wrapper.Object := Get_Request (Handler.Object (This));
            Info    : Types.Request_Type;
         begin
         -- Info is initialized by the following statement
            Wrapper.Get_Request (Info);
            Env.all.Set_Header (Info);
         end;
      end if;
      Handler.Object (This).Handle (Env);
   end Handle;

-- private
   procedure Initialize (
      This_Reference : Request.Reference;
      Next           : Handler.Reference;
      Req            : Req_Wrapper.Object) is
   begin
      Initialize (Handler.Object (This_Reference.all), Next, Req);
   end Initialize;

end Interface_Layer.Service.Pipelines.Handler.Request;
