package Interface_Layer.Service.Pipelines.Handler.Data.Traveller is

   type Object is
      new Interface_Layer.Service.Pipelines.Handler.Object
   with null record;
   type Reference is access all Traveller.Object'Class;

   function Create (
      Next : Handler.Reference;
      Req  : Req_Wrapper.Object := Empty_Request)
         return Traveller.Reference;

   overriding
   procedure Handle (This : Traveller.Object);

   overriding
   procedure Handle (This : Traveller.Object; Env : Envelope.Reference);

private
   procedure Initialize (
      This_Reference : Traveller.Reference;
      Next           : Handler.Reference;
      Req            : Req_Wrapper.Object);

end Interface_Layer.Service.Pipelines.Handler.Data.Traveller;