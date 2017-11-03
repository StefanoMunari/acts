package Interface_Layer.Service.Pipelines.Handler.Request is

   type Object is
      new Interface_Layer.Service.Pipelines.Handler.Object
   with null record;
   type Reference is access all Request.Object'Class;

   function Create (
      Next : Handler.Reference;
      Req  : Req_Wrapper.Object := Empty_Request)
   return Request.Reference;

   overriding
   procedure Handle (This : Request.Object);

   overriding
   procedure Handle (This : Request.Object; Env : Envelope.Reference);

private
   procedure Initialize (
      This_Reference : Request.Reference;
      Next           : Handler.Reference;
      Req            : Req_Wrapper.Object);

end Interface_Layer.Service.Pipelines.Handler.Request;
