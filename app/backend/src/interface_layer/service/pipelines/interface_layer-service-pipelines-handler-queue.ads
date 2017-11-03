package Interface_Layer.Service.Pipelines.Handler.Queue is

   type Object is
      new Interface_Layer.Service.Pipelines.Handler.Object
   with null record;
   type Reference is access all Queue.Object'Class;

   function Create
   return Queue.Reference;

   overriding
   procedure Handle (This : Queue.Object);
   overriding
   procedure Handle (This : Queue.Object; Env : Envelope.Reference);

   No_Envelope_To_Enqueue : exception;
private
   procedure Initialize (
      This_Reference :    Queue.Reference;
      Next           : in Interface_Layer.Service.Pipelines.Handler.Reference;
      Req            : in Req_Wrapper.Object := Empty_Request);

end Interface_Layer.Service.Pipelines.Handler.Queue;
