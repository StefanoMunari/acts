-- local
with Interface_Layer.Presentation.Envelope;
with Interface_Layer.Utils.Types;
with Interface_Layer.Containers.Queues;

with Shared.Indefinite_String_Map;
with Shared.Shared_References_App_Wrapper;

-- core
with Ada.Containers;

package body Interface_Layer.Service.Pipelines.Handler.Data.Other is
   use Req_Wrapper; -- view "=" operator
   use Interface_Layer.Containers.Queues; -- view queues instances
   use Ada.Containers;  -- view "=" operator

   package Envelope renames Interface_Layer.Presentation.Envelope;
   package Types renames Interface_Layer.Utils.Types;
   package String_Map renames Shared.Indefinite_String_Map;

   function Create (
      Next : Handler.Reference;
      Req  : Req_Wrapper.Object := Empty_Request)
      return Other.Reference
   is
      Instance : Other.Reference;
   begin
      Instance := new Other.Object;
      Initialize (Instance, Next, Req);
      return Instance;
   end Create;

   procedure Handle (This : Other.Object) is
   begin
      if Has_Request (Handler.Object (This))
      then
         declare
            Env : Envelope.Reference := Envelope.Create;
         begin
            This.Handle (Env);
         end;
      else
         Handler.Object (This).Handle;
      end if;
   end Handle;

   procedure Handle (This : Other.Object; Env : Envelope.Reference) is
   begin
      if Has_Request (Handler.Object (This))
      then
         declare
            Wrapper   : Req_Wrapper.Object
               := Get_Request (Handler.Object (This));
            Info_Type : Types.Data_Type;
            Info      : App_Wrapper_Pkg.Reference;
         begin
            pragma Warnings (Off, "may be referenced before it has a value");
            Wrapper.Get_Request (Info_Type);
            pragma Warnings (On, "may be referenced before it has a value");
            Wrapper.Get_Request (Info);
            Env.Set_Header (Info_Type);
         -- pass parameter by copy
            Env.Set_Message (Info.Get_Data);
            Info.Finalize;
         end;
      end if;
      Handler.Object (This).Handle (Env);
   end Handle;

-- private
   procedure Initialize (
      This_Reference : Other.Reference;
      Next           : Handler.Reference;
      Req            : Req_Wrapper.Object) is
   begin
      Initialize (Handler.Object (This_Reference.all), Next, Req);
   end Initialize;

end Interface_Layer.Service.Pipelines.Handler.Data.Other;
