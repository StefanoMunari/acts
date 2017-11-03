package body Interface_Layer.Service.Pipelines.Barrier is

   protected body Object is

      procedure Open_Barrier
      is
      begin
         Open := True;
      end Open_Barrier;

      entry Wait
      when Open is
      begin
         null;
      end Wait;

   end Object;

end Interface_Layer.Service.Pipelines.Barrier;
