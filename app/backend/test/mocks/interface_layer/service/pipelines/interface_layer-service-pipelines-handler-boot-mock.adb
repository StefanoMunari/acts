package body Interface_Layer.Service.Pipelines.Handler.Boot.Mock is

   function Create return Boot.Mock.Reference
   is
      Instance : Boot.Mock.Reference := new Boot.Mock.Object;
   begin
      return Instance;
   end Create;

end Interface_Layer.Service.Pipelines.Handler.Boot.Mock;
