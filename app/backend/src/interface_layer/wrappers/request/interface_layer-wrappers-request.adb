-- core
with Ada.Unchecked_Deallocation;
with Ada.Exceptions;

package body Interface_Layer.Wrappers.Request is
   package Exc renames Ada.Exceptions;

   use Interface_Layer.Wrappers.Application;

   function Empty return Request.Object
   is
      Instance : Request.Object;
   begin
      return Instance;
   end Empty;

   function Create (Request_0 : in App_Wrapper_Pkg.Reference)
   return Request.Object
   is
      Instance : Request.Object;
   begin
      Instance.Wrapper_Ref := Request_0;
      return Instance;
   end Create;

   function Create (Request_0 : in App_Wrapper_Pkg.Reference;
                    Request_1 : in Types.Data_Type)
   return Request.Object
   is
      Instance : Request.Object;
   begin
      Instance.Wrapper_Ref := Request_0;
      Instance.Data_T := Request_1;
      return Instance;
   end Create;

   function Create (Request_0 : in Callback_Pair_Pkg.Object;
                    Request_1 : in SU.Unbounded_String)
   return Request.Object
   is
      Instance : Request.Object;
   begin
      Instance.Callbacks      := Request_0;
      Instance.Correlation_Id := Request_1;
      return Instance;
   end Create;

   function Create (Request_0 : in Types.Request_Type)
   return Request.Object
   is
      Instance : Request.Object;
   begin
      Instance.Req_T := Request_0;
      return Instance;
   end Create;

   function Create (Request_0 : in Types.Data_Type)
   return Request.Object
   is
      Instance : Request.Object;
   begin
      Instance.Data_T := Request_0;
      return Instance;
   end Create;

   function Create (Request_0 : in Types.Call_Type)
   return Request.Object
   is
      Instance : Request.Object;
   begin
      Instance.Call_T := Request_0;
      return Instance;
   end Create;

   function Create (Request_0 : in SU.Unbounded_String)
   return Request.Object
   is
      Instance : Request.Object;
   begin
      Instance.Correlation_Id := Request_0;
      return Instance;
   end Create;

   function Create (Request_0 : in Recipient_Type)
   return Request.Object
   is
      Instance : Request.Object;
   begin
      Instance.Recipient := Request_0;
      return Instance;
   end Create;

   procedure Get_Request (
      This    :        Request.Object;
      Request : in out App_Wrapper_Pkg.Reference)
   is
   begin
      Request := This.Wrapper_Ref;
   end Get_Request;

   procedure Get_Request (
      This    :     Request.Object;
      Request : out Callback_Pair_Pkg.Object) is
   begin
      Request := This.Callbacks;
   end Get_Request;

   procedure Get_Request (
      This    :     Request.Object;
      Request : out Types.Request_Type) is
   begin
      Request := This.Req_T;
   end Get_Request;

   procedure Get_Request (
      This    :     Request.Object;
      Request : out Types.Data_Type) is
   begin
      Request := This.Data_T;
   end Get_Request;

   procedure Get_Request (
      This    :     Request.Object;
      Request : out Types.Call_Type) is
   begin
      Request := This.Call_T;
   end Get_Request;

   procedure Get_Request (
      This    :     Request.Object;
      Request : out Recipient_Type) is
   begin
      Request := This.Recipient;
   end Get_Request;

   procedure Get_Request (
      This    :     Request.Object;
      Request : out SU.Unbounded_String) is
   begin
      Request := This.Correlation_Id;
   end Get_Request;

   procedure Initialize (This : in out Request.Object)
   is
   begin
      null;
   end Initialize;

   procedure Finalize (This : in out Request.Object)
   is
   begin
      null;
   end Finalize;

end Interface_Layer.Wrappers.Request;
