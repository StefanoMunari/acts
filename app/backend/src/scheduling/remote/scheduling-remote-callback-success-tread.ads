------------------------------------------------------------------------------
-- @author  <gianmarco.midena@gmail.com>
-- @author  <stefanomunari.sm@gmail.com>
-- @author  <valle.sebastiano93@gmail.com>
-- @context application-backend::scheduling-remote-callback-success-tread
-- @purpose Type which executes success callback for Tread operations
-- @interface @inherit
-- @dependencies application-backend::active-agent
--               application-backend::reactive-district
-- @details Interface.
------------------------------------------------------------------------------

with Active.Agent;
with Active.People_Carrier.Utils;

with Interface_Layer.Remote.Stub;
with Interface_Layer.Wrappers.Application;
with Interface_Layer.Wrappers.Application.Abstract_Factory;

with Reactive;
with Reactive.District;

package Scheduling.Remote.Callback.Success.Tread is

   package District_Pkg renames Reactive.District;
   package PC_Utils_Pkg renames Active.People_Carrier.Utils;
   package Stub_Pkg     renames Interface_Layer.Remote.Stub;
   package App_Wrapper_Pkg
      renames Interface_Layer.Wrappers.Application;
   package Abs_Factory_Pkg
      renames App_Wrapper_Pkg.Abstract_Factory;

   use Reactive.Infra_Id_Type;

   type Object is new Success.Object
   with private;
   type Reference is access all Tread.Object'Class;

   function Create (
      Traveller : in     Active.Agent.Agent_Id;
      Treadable : in     Infra_Id;
      District  : access District_Pkg.Object'Class := null;
      PC_Utils  : access PC_Utils_Pkg.Object'Class := null;
      W_Factory : access Abs_Factory_Pkg.Object'Class:= null;
      Stub      : access Stub_Pkg.Object'Class := null)
   return Success.Reference;

   overriding
   procedure Execute (This : in Success.Tread.Object);

private
   type Object is new Success.Object
   with record
      Traveller : Active.Agent.Agent_Id;
      Treadable : Infra_Id;
      District  : access District_Pkg.Object'Class := null;
      PC_Utils  : access PC_Utils_Pkg.Object'Class := null;
      W_Factory : access Abs_Factory_Pkg.Object'Class:= null;
      Stub      : access Stub_Pkg.Object'Class := null;
   end record;

end Scheduling.Remote.Callback.Success.Tread;
