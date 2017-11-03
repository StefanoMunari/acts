-- core
with Ada.Containers;

-- local
with Interface_Layer.Containers.Queues;
with Interface_Layer.Presentation.Envelope;
with Interface_Layer.Utils.Types;

package body Interface_Layer.Service.Pipelines.Handler.Correlation_Id is
   use Req_Wrapper; -- view "=" operator
   use Interface_Layer.Containers.Queues; -- view queues instances
   use Ada.Containers;  -- view "=" operator

   package Envelope renames Interface_Layer.Presentation.Envelope;
   package Types    renames Interface_Layer.Utils.Types;

   function Create (
      Next : Handler.Reference;
      Req  : Req_Wrapper.Object := Empty_Request)
   return Correlation_Id.Reference
   is
      Instance : Correlation_Id.Reference;
   begin
      Instance := new Correlation_Id.Object;
      Initialize (Instance, Next, Req);
      return Instance;
   end Create;

   procedure Handle (This : Correlation_Id.Object) is
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

   overriding
   procedure Handle (This : Correlation_Id.Object; Env : Envelope.Reference) is
   begin
      if Has_Request (Handler.Object (This))
      then
         declare
            Wrapper : Req_Wrapper.Object
               := Get_Request (Handler.Object (This));
            Info    : SU.Unbounded_String;
         begin
            Wrapper.Get_Request (Info);
            Env.all.Set_Header (SU.To_String (Info));
         end;
      end if;
      Handler.Object (This).Handle (Env);
   end Handle;

-- private
   procedure Initialize (
      This_Reference :    Correlation_Id.Reference;
      Next           : in Handler.Reference;
      Req            : in Req_Wrapper.Object) is
   begin
      Initialize (Handler.Object (This_Reference.all), Next, Req);
   end Initialize;

end Interface_Layer.Service.Pipelines.Handler.Correlation_Id;
