package body Interface_Layer.Service.Pipelines.Handler.Rendezvous.Mock is

   function Create (Next : Handler.Reference;
                    Req  : Req_Wrapper.Object := Empty_Request)
   return Rendezvous.Mock.Reference
   is
      Instance : Rendezvous.Mock.Reference
         := new Rendezvous.Mock.Object;
   begin
      Instance.Mocked_Values.Next := Next;
      Instance.Mocked_Values.Next_Existence := True;
      Instance.Mocked_Values.Req := Req;
      Instance.Mocked_Values.Next_Existence := False;
      return Instance;
   end;

end Interface_Layer.Service.Pipelines.Handler.Rendezvous.Mock;
