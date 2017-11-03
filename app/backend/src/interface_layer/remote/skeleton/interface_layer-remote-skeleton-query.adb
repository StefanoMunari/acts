separate (Interface_Layer.Remote.Skeleton)

procedure Query (
   This    : in     Skeleton.Object;
   Message : in out Interface_Wrapper.Object)
is
   procedure Free is new  Ada.Unchecked_Deallocation (
      Query_Decoder.Object'Class, Query_Decoder.Reference);

   Message_SU     : SU.Unbounded_String := Message.Get_Data;
   Message_Str    : String := SU.To_String (Message_SU);
   Decoder        : Query_Decoder.Reference
      := Query_Decoder.Create (Message_Str);
   Correlation_Id : SU.Unbounded_String := Decoder.Decode_Correlation_Id;
   Reply_Wrapper  : App_Wrapper_Pkg.Reference;
begin
   Reply_Wrapper := This.Dispatcher.Dispatch (Message_Str);

   Reply_Chain:
      declare
         Queue_Handler : Interface_Layer.Service.Pipelines.Handler.Reference :=
            This.Handler_Factory.Create_Queue_Handler;
         Data_Handler  : Interface_Layer.Service.Pipelines.Handler.Reference :=
            This.Handler_Factory.Create_Data_Other_Handler (
               Queue_Handler, Reply_Wrapper, Types.Ack);
         Id_Handler    : Interface_Layer.Service.Pipelines.Handler.Reference :=
            This.Handler_Factory.Create_Correlation_Id_Handler (
               Data_Handler, Correlation_Id);
         Call_Handler  : Interface_Layer.Service.Pipelines.Handler.Reference :=
            This.Handler_Factory.Create_Call_Handler (
               Id_Handler, Types.SYNC);
         Req_Handler   : Interface_Layer.Service.Pipelines.Handler.Reference :=
            This.Handler_Factory.Create_Request_Handler (
               Call_Handler, Types.QUERY);
      begin
      -- Activate the chain
         Req_Handler.Handle;
      end Reply_Chain;

-- Free resources
   Message.Finalize;
   Free (Decoder);

end Query;
