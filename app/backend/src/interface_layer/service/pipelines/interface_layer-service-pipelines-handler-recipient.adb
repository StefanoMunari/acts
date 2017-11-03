-- core
with Ada.Containers;

-- local
with Interface_Layer.Presentation.Envelope;
with Interface_Layer.Utils.Types;
with Interface_Layer.Containers.Queues;

with Reactive;

with Shared.Indefinite_String_Map;

package body Interface_Layer.Service.Pipelines.Handler.Recipient is
   use Req_Wrapper; -- view "=" operator
   use Interface_Layer.Containers.Queues; -- view queues instances
   use Ada.Containers;  -- view "=" operator
   use Reactive.Infra_Id_Type;

   package Envelope renames Interface_Layer.Presentation.Envelope;
   package Types renames Interface_Layer.Utils.Types;
   package String_Map renames Shared.Indefinite_String_Map;

   use Types.Recipient_Type_Pkg;

   function Create (
      Next : Handler.Reference;
      Req  : Req_Wrapper.Object := Empty_Request)
   return Recipient.Reference
   is
      Instance : Recipient.Reference;
   begin
      Instance := new Recipient.Object;
      Initialize (Instance, Next, Req);
      return Instance;
   end Create;

   procedure Handle (This : Recipient.Object) is
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

   procedure Handle (This : Recipient.Object; Env : Envelope.Reference) is
   begin
      if Has_Request (Handler.Object (This))
      then
         declare
            Wrapper        : Req_Wrapper.Object
               := Get_Request (Handler.Object (This));
            Recipient : Recipient_Type;
         begin
            Wrapper.Get_Request (Recipient);
            Env.Set_Header (Recipient);
         end;
      end if;
      Handler.Object (This).Handle (Env);
   end Handle;

-- private
   procedure Initialize (
      This_Reference : Recipient.Reference;
      Next           : Handler.Reference;
      Req            : Req_Wrapper.Object) is
   begin
      Initialize (Handler.Object (This_Reference.all), Next, Req);
   end Initialize;

end Interface_Layer.Service.Pipelines.Handler.Recipient;
