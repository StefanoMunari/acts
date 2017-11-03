package Interface_Layer.Service.Pipelines.Handler.Recipient is

   type Object is
      new Interface_Layer.Service.Pipelines.Handler.Object
   with null record;
   type Reference is access all Recipient.Object'Class;

   function Create (
      Next : Handler.Reference;
      Req  : Req_Wrapper.Object := Empty_Request)
   return Recipient.Reference;

   overriding
   procedure Handle (This : Recipient.Object);

   overriding
   procedure Handle (This : Recipient.Object; Env : Envelope.Reference);

private
   procedure Initialize (
      This_Reference : Recipient.Reference;
      Next           : Handler.Reference;
      Req            : Req_Wrapper.Object);

end Interface_Layer.Service.Pipelines.Handler.Recipient;
