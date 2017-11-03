package Interface_Layer.Service.Pipelines.Handler.Rendezvous is

   type Object is
      new Interface_Layer.Service.Pipelines.Handler.Object
   with null record;
   type Reference is access all Rendezvous.Object'Class;

   function Create (
      Next : Handler.Reference;
      Req  : Req_Wrapper.Object := Empty_Request)
   return Rendezvous.Reference;

   overriding
   procedure Handle (This : Rendezvous.Object);

   overriding
   procedure Handle (This : Rendezvous.Object; Env : Envelope.Reference);

private
   procedure Initialize (
      This_Reference : Rendezvous.Reference;
      Next           : Handler.Reference;
      Req            : Req_Wrapper.Object);

end Interface_Layer.Service.Pipelines.Handler.Rendezvous;
