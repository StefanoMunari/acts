-- core
with Ada.Unchecked_Deallocation;

package body Interface_Layer.Service.Pipelines.Handler is
   use Req_Wrapper; -- make "=" operator visible

   function Has_Request (This : Handler.Object)
      return Boolean is
   begin
      return not (This.Req = Handler.Empty_Request);
   end Has_Request;

   procedure Handle (This : Handler.Object) is
   begin
      if This.Next = null then
         raise Null_Handler;
      else
         This.Next.all.Handle;
      end if;
   end Handle;

   procedure Handle (This : Handler.Object; Env : Envelope.Reference) is
   begin
      if This.Next = null then
         raise Null_Handler;
      else
         This.Next.all.Handle (Env);
      end if;
   end Handle;

   procedure Finalize (
      Handler_Ref : in out Handler.Reference)
   is
      procedure Free is new  Ada.Unchecked_Deallocation (
         Handler.Object'Class, Handler.Reference);
   begin
      Free (Handler_Ref);
   end Finalize;

-- private

   procedure Initialize (
      This : in out Interface_Layer.Service.Pipelines.Handler.Object;
      Next :        Handler.Reference;
      Req  :        Req_Wrapper.Object := Empty_Request) is
   begin
      This.Next := Next;
      This.Req  := Req;
   end Initialize;

   function Get_Request (
      This : Handler.Object)
   return  Req_Wrapper.Object is
    begin
      return This.Req;
   end Get_Request;

end Interface_Layer.Service.Pipelines.Handler;
