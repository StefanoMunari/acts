-- local
with Interface_Layer.Presentation.Envelope;
with Interface_Layer.Wrappers.Request;

package Interface_Layer.Service.Pipelines.Handler is
   package Req_Wrapper renames Interface_Layer.Wrappers.Request;
   package Envelope renames Interface_Layer.Presentation.Envelope;
   -- circular dependency - suppress a false positive result
--   pragma Suppress (Elaboration_Check, On => Req_Wrapper);
   pragma Suppress (Elaboration_Check);

   type Object is abstract tagged private;
   type Reference is access all Handler.Object'Class;

   function Has_Request (This : Handler.Object) return Boolean;

   procedure Handle (This : Handler.Object);

   procedure Handle (
      This : Handler.Object;
      Env  : Envelope.Reference);

   procedure Finalize (Handler_Ref : in out Handler.Reference);

   Empty_Request : Req_Wrapper.Object := Req_Wrapper.Empty;
   Null_Handler  : exception;

private
   procedure Initialize (
      This : in out Handler.Object;
      Next :        Handler.Reference;
      Req  :        Req_Wrapper.Object := Empty_Request);

   function Get_Request (This : Handler.Object)
   return  Req_Wrapper.Object;

   type Object is abstract tagged
   record
      Next : Handler.Reference;
      Req  : Req_Wrapper.Object;
   end record;

end Interface_Layer.Service.Pipelines.Handler;
