separate (Interface_Layer.Remote.Skeleton)

procedure Enter (
   This   : in     Skeleton.Object;
   Entity : in out Interface_Wrapper.Object)
is
   procedure Free is new  Ada.Unchecked_Deallocation (
      App_Wrapper_Pkg.Object'Class, App_Wrapper_Pkg.Reference);
   District_Ref   : District.Reference := District.Get_Instance;
   Traveller_Ref  : Active.Traveller.Reference := Entity.Get_Data;
   Correlation_Id : SU.Unbounded_String := Traveller_Ref.Get_Id;
   Old_Position   : Infra_Id := Traveller_Ref.Get_Position;
   Queue_Handler  : Interface_Layer.Service.Pipelines.Handler.Reference :=
      This.Handler_Factory.Create_Queue_Handler;
   Id_Handler     : Interface_Layer.Service.Pipelines.Handler.Reference :=
      This.Handler_Factory.Create_Correlation_Id_Handler (
         Queue_Handler, Correlation_Id);
   Call_Handler   : Interface_Layer.Service.Pipelines.Handler.Reference :=
      This.Handler_Factory.Create_Call_Handler (
         Id_Handler, Types.ASYNC);
   Data_Handler   : Interface_Layer.Service.Pipelines.Handler.Reference;
   Added          : Boolean;
   Removed        : Boolean;
   Entered        : Boolean := False;
   Result_Wrapper : App_Wrapper_Pkg.Reference;
   Next_Step      : Infra_Id := Traveller_Ref.Look_Ahead_Step (0);
begin

-- be safe against retransmissions
   if District_Ref.Contains_Traveller (Traveller_Ref.Get_Id) then
   -- Wrap result
      Result_Wrapper := This.Wrapper_Factory.Create_Wrapper (Entered);
   -- Complete chain configuration with the result
      Data_Handler :=
         This.Handler_Factory.Create_Data_Traveller_Handler (
            Call_Handler, Result_Wrapper);
   -- Activate the chain
      Data_Handler.Handle;
   -- Free resources
      Entity.Finalize;
      return;
   end if;

-- Pass the Traveller to District
   District_Ref.Add_Traveller (Traveller_Ref, Added);

   Traveller_Ref.Act;

   if Traveller_Ref.Get_Position = Old_Position then
      Entered := False;
      District_Ref.Remove_Traveller (Traveller_Ref.Get_Id, Removed);
   else
   -- FALSE BRANCH: traveller entered the new district
      Entered := True;
   end if;

-- Wrap result
   Result_Wrapper := This.Wrapper_Factory.Create_Wrapper (Entered);

   Reply_Chain:
      declare
         Queue_Handler : Interface_Layer.Service.Pipelines.Handler.Reference :=
            This.Handler_Factory.Create_Queue_Handler;
         Data_Handler  : Interface_Layer.Service.Pipelines.Handler.Reference :=
            This.Handler_Factory.Create_Data_Other_Handler (
               Queue_Handler, Result_Wrapper, Types.Ack);
         Id_Handler    : Interface_Layer.Service.Pipelines.Handler.Reference :=
            This.Handler_Factory.Create_Correlation_Id_Handler (
               Data_Handler, Correlation_Id);
         Call_Handler  : Interface_Layer.Service.Pipelines.Handler.Reference :=
            This.Handler_Factory.Create_Call_Handler (
               Id_Handler, Types.SYNC);
         Req_Handler   : Interface_Layer.Service.Pipelines.Handler.Reference :=
            This.Handler_Factory.Create_Request_Handler (
               Call_Handler, Types.ENTER);
      begin
      -- Activate the chain
         Req_Handler.Handle;
      end Reply_Chain;

-- Free resources
   Entity.Finalize;
   Free (Result_Wrapper);

end Enter;
