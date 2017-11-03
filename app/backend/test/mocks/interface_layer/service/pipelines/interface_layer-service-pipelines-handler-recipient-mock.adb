package body Interface_Layer.Service.Pipelines.Handler.Recipient.Mock is

   function Create (
      Next : Handler.Reference;
      Req  : Req_Wrapper.Object := Empty_Request)
   return Recipient.Mock.Reference
   is
      Instance : Recipient.Mock.Reference := new Recipient.Mock.Object;
   begin
      Instance.Mocked_Values.Next := Next;
      Instance.Mocked_Values.Next_Existence := True;
      Instance.Mocked_Values.Req := Req;
      Instance.Mocked_Values.Next_Existence := False;
      return Instance;
   end Create;

end Interface_Layer.Service.Pipelines.Handler.Recipient.Mock;
