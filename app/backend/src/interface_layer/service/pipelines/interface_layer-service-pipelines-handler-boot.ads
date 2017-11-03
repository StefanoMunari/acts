package Interface_Layer.Service.Pipelines.Handler.Boot is

   type Object is
      new Interface_Layer.Service.Pipelines.Handler.Object
   with null record;
   type Reference is access all Boot.Object'Class;

   function Create
   return Boot.Reference;

   overriding
   procedure Handle (This : Boot.Object);
   overriding
   procedure Handle (This : Boot.Object; Env : Envelope.Reference);
private
   procedure Initialize (
      This_Reference :    Boot.Reference;
      Next           : in Interface_Layer.Service.Pipelines.Handler.Reference;
      Req            : in Req_Wrapper.Object := Empty_Request);

end Interface_Layer.Service.Pipelines.Handler.Boot;
