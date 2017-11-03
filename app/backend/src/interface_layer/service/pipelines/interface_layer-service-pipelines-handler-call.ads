package Interface_Layer.Service.Pipelines.Handler.Call is

   type Object is
      new Interface_Layer.Service.Pipelines.Handler.Object
   with null record;
   type Reference is access all Call.Object'Class;

   function Create (Next : Handler.Reference;
                    Req  : Req_Wrapper.Object := Empty_Request)
   return Call.Reference;

   overriding
   procedure Handle (This : Call.Object);

   overriding
   procedure Handle (This : Call.Object; Env : Envelope.Reference);

private
   procedure Initialize (
      This_Reference :    Call.Reference;
      Next           : in Handler.Reference;
      Req            : in Req_Wrapper.Object);

end Interface_Layer.Service.Pipelines.Handler.Call;
