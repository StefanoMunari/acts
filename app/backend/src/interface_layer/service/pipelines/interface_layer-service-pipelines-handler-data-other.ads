package Interface_Layer.Service.Pipelines.Handler.Data.Other is

   type Object is
      new Interface_Layer.Service.Pipelines.Handler.Object
   with null record;
   type Reference is access all Other.Object'Class;

   function Create (
      Next : Handler.Reference;
      Req  : Req_Wrapper.Object := Empty_Request)
   return Other.Reference;

   overriding
   procedure Handle (This : Other.Object);

   overriding
   procedure Handle (This : Other.Object; Env : Envelope.Reference);

private
   procedure Initialize (
      This_Reference : Other.Reference;
      Next           : Handler.Reference;
      Req            : Req_Wrapper.Object);

end Interface_Layer.Service.Pipelines.Handler.Data.Other;
