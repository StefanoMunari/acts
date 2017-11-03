package Interface_Layer.Service.Pipelines.Handler.Correlation_Id is

   type Object is
      new Interface_Layer.Service.Pipelines.Handler.Object
   with null record;
   type Reference is access all Correlation_Id.Object'Class;

   function Create (Next : Handler.Reference;
                    Req  : Req_Wrapper.Object := Empty_Request)
   return Correlation_Id.Reference;

   overriding
   procedure Handle (This : Correlation_Id.Object);

   overriding
   procedure Handle (This : Correlation_Id.Object; Env : Envelope.Reference);

private
   procedure Initialize (
    This_Reference :    Correlation_Id.Reference;
    Next           : in Interface_Layer.Service.Pipelines.Handler.Reference;
    Req            : in Req_Wrapper.Object);

end Interface_Layer.Service.Pipelines.Handler.Correlation_Id;
