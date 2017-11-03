package body Interface_Layer.Service.Pipelines.Handler.Correlation_Id.Mock is

   function Create (Next : Handler.Reference;
                    Req  : Req_Wrapper.Object := Empty_Request)
   return Correlation_Id.Mock.Reference
   is
      Instance : Correlation_Id.Mock.Reference
         := new Correlation_Id.Mock.Object;
   begin
      Instance.Mocked_Values.Next := Next;
      Instance.Mocked_Values.Next_Existence := True;
      Instance.Mocked_Values.Req := Req;
      Instance.Mocked_Values.Next_Existence := False;
      return Instance;
   end;

end Interface_Layer.Service.Pipelines.Handler.Correlation_Id.Mock;
