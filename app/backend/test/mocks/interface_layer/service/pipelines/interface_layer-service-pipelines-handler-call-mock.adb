package body Interface_Layer.Service.Pipelines.Handler.Call.Mock is

   function Create (Next : Handler.Reference;
                    Req  : Req_Wrapper.Object := Empty_Request)
   return Call.Mock.Reference
   is
      Instance : Call.Mock.Reference := new Call.Mock.Object;
   begin
      Instance.Mocked_Values.Next := Next;
      Instance.Mocked_Values.Next_Existence := True;
      Instance.Mocked_Values.Req := Req;
      Instance.Mocked_Values.Next_Existence := False;
      return Instance;
   end;

   overriding
   procedure Handle (This : Handler.Call.Mock.Object) is
   begin
      if not This.Return_Values.Handle_Set then
         return;
      end if;
   -- TODO: Otherwise, set the rendezvous at `This.Handle
   end;

   overriding
   procedure Handle (This : Handler.Call.Mock.Object;
                     Env  : Envelope.Reference) is
   begin
      if not This.Return_Values.Handle_Set then
         return;
      end if;
   -- TODO: Otherwise, set the rendezvous at `This.Handle
   end;

   not overriding
   procedure Set_Answer_For_Rendezvous (This   : in out Call.Mock.Object;
                                        Answer : in Boolean) is
   begin
      This.Return_Values.Handle_Set := True;
      This.Return_Values.Handle     := Answer;
   end;

end Interface_Layer.Service.Pipelines.Handler.Call.Mock;
