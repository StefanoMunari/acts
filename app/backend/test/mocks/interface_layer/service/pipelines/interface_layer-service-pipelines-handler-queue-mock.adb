package body Interface_Layer.Service.Pipelines.Handler.Queue.Mock is

   function Create return Queue.Mock.Reference
   is
      Instance : Queue.Mock.Reference := new Queue.Mock.Object;
   begin
      return Instance;
   end;

end Interface_Layer.Service.Pipelines.Handler.Queue.Mock;
