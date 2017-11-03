package Interface_Layer.Service.Pipelines.Barrier is

   protected Object is
      procedure Open_Barrier;
      entry Wait;
   private
      Open : Boolean := False;
   end Object;

end Interface_Layer.Service.Pipelines.Barrier;
