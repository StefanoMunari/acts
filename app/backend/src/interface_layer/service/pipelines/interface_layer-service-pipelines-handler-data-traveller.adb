-- local
with Active.Traveller;

with Interface_Layer.Presentation.Envelope;
with Interface_Layer.Utils.Types;
with Interface_Layer.Containers.Queues;

with Shared.Indefinite_String_Map;

-- core
with Ada.Containers;


package body Interface_Layer.Service.Pipelines.Handler.Data.Traveller is
   use Req_Wrapper; -- view "=" operator
   use Ada.Containers; -- view "=" operator
   use Interface_Layer.Containers.Queues; -- view queues instances

   package Envelope renames Interface_Layer.Presentation.Envelope;
   package Types renames Interface_Layer.Utils.Types;
   package String_Map renames Shared.Indefinite_String_Map;

   function Create (Next : Handler.Reference;
                    Req  : Req_Wrapper.Object := Empty_Request)
      return Traveller.Reference
   is
      Instance : Traveller.Reference;
   begin
      Instance := new Traveller.Object;
      Initialize (Instance, Next, Req);
      return Instance;
   end Create;

   procedure Handle (This : Traveller.Object) is
   begin
      if Has_Request (Handler.Object (This)) then
         declare
            Env : Envelope.Reference := Envelope.Create;
         begin
            This.Handle (Env);
         end;
      else
         Handler.Object (This).Handle;
      end if;
   end Handle;

   procedure Handle (This : Traveller.Object; Env : Envelope.Reference) is
   begin
      if Has_Request (Handler.Object (This)) then
         declare
            Wrapper : Req_Wrapper.Object := This.Get_Request;
            Info    : App_Wrapper_Pkg.Reference;
         begin
            Wrapper.Get_Request (Info);
         -- pass parameter by copy
            Env.Set_Header (Info.Get_Concrete_Data_Type);
            Env.Set_Message (Info.Get_Data);
         end;
      end if;
      Handler.Object (This).Handle (Env);
   end Handle;

-- private
   procedure Initialize (
      This_Reference : Traveller.Reference;
      Next           : Handler.Reference;
      Req            : Req_Wrapper.Object) is
   begin
      Initialize (Handler.Object (This_Reference.all), Next, Req);
   end Initialize;

end Interface_Layer.Service.Pipelines.Handler.Data.Traveller;
