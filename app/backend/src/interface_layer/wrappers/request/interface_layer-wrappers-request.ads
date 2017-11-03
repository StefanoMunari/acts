--------------------------------------------------------------------------------
-- @author <stefanomunari.sm@gmail.com>
-- @context application-backend::interface_layer
-- @purpose wraps every information that the application needs to pass to the
--          lower layers (i.e. interface-layer) through the handlers.
--          Note that this class is necessary to provide
--          transparency from the specific objects/info of
--          the application to the interface_layer-service-handler hierarchy
--          which can abstract from the specific application level
--          objects/information and handle Request objects.
-- @interface Empty (arity = 0) :
--              provides an empty request wrapper. use it instead of Null
--            Create (arity = 1 || arity = 2) :
--              provides a class constructor, one for each information which
--              the wrapper needs to wrap
--            Get_Request (arity = 2) :
--              fills the request pass as input output parameter with the
--              corresponding which the Request instance (this) is
--              actually wrapping
--            Finalize (arity = 1 : Object) :
--              free the resources dynamically allocated
--              (e.g. references) associated with this class instance
--            Finalize (arity = 1 : Reference) :
--              free the resources dynamically allocated
--              (e.g. references) associated with this reference
--              then free the memory pointed by the reference
--------------------------------------------------------------------------------
-- core
with Ada.Finalization;
with Ada.Strings.Unbounded;

with Interface_Layer.Wrappers.Application;
with Interface_Layer.Utils.Types;

with Reactive;

with Shared.Callback_Pair;

package Interface_Layer.Wrappers.Request is

   package Types             renames Interface_Layer.Utils.Types;
   package App_Wrapper_Pkg   renames Interface_Layer.Wrappers.Application;
   package Callback_Pair_Pkg renames Shared.Callback_Pair;
   package SU                renames Ada.Strings.Unbounded;

   use Reactive.Infra_Id_Type;
   use Types.Recipient_Type_Pkg;

   type Object is new Ada.Finalization.Controlled with private;
   type Reference is access all Request.Object'Class;

   function Empty -- Empty Wrapper
     return Request.Object;

   function Create (
      Request_0 : in App_Wrapper_Pkg.Reference)
   return Request.Object;
   function Create (
      Request_0 : in App_Wrapper_Pkg.Reference;
      Request_1 : in Types.Data_Type)
   return Request.Object;
   function Create (
      Request_0 : in Callback_Pair_Pkg.Object;
      Request_1 : in SU.Unbounded_String)
   return Request.Object;
   function Create (Request_0 : in Types.Request_Type)
   return Request.Object;
   function Create (Request_0 : in Types.Data_Type)
   return Request.Object;
   function Create (Request_0 : in Types.Call_Type)
   return Request.Object;
   function Create (Request_0 : in SU.Unbounded_String)
   return Request.Object;
   function Create (Request_0 : in Recipient_Type)
   return Request.Object;

   procedure Get_Request (
      This    :        Request.Object;
      Request : in out App_Wrapper_Pkg.Reference);
   procedure Get_Request (
      This    :     Request.Object;
      Request : out Callback_Pair_Pkg.Object);
   procedure Get_Request (
      This    :     Request.Object;
      Request : out Types.Request_Type);
   procedure Get_Request (
      This    :     Request.Object;
      Request : out Types.Data_Type);
   procedure Get_Request (
      This    :     Request.Object;
      Request : out Types.Call_Type);
   procedure Get_Request (
      This    :     Request.Object;
      Request : out Recipient_Type);
   procedure Get_Request (
      This    :     Request.Object;
      Request : out SU.Unbounded_String);

   overriding
   procedure Initialize (This : in out Request.Object);
   overriding
   procedure Finalize   (This : in out Request.Object);

private
   type Object is new Ada.Finalization.Controlled with
   record
      Wrapper_Ref    : App_Wrapper_Pkg.Reference;
      Callbacks      : Callback_Pair_Pkg.Object;
      Req_T          : Types.Request_Type;
      Data_T         : Types.Data_Type;
      Call_T         : Types.Call_Type;
      Correlation_Id : SU.Unbounded_String;
      Recipient      : Recipient_Type;
   end record;

end Interface_Layer.Wrappers.Request;
