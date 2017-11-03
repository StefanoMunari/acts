------------------------------------------------------------------------------
-- @author  <gianmarco.midena@gmail.com>
-- @author  <stefanomunari.sm@gmail.com>
-- @author  <valle.sebastiano93@gmail.com>
-- @context application-backend::interface_layer-containers-callback_pair
-- @purpose <Success, Failure> pair
-- @interface Create (Success, Failure)
--              ctor
-- @dependencies -
-- @details -
------------------------------------------------------------------------------

with Scheduling.Remote.Callback;
with Scheduling.Remote.Callback.Failure;
with Scheduling.Remote.Callback.Success;

-- core
with Ada.Finalization;

package Shared.Callback_Pair is

   package Failure_Pkg renames Scheduling.Remote.Callback.Failure;
   package Success_Pkg renames Scheduling.Remote.Callback.Success;

   type Object is
   new Ada.Finalization.Controlled
   with record
      Failure : Failure_Pkg.Reference;
      Success : Success_Pkg.Reference;
   end record;
   type Reference is access all Callback_Pair.Object'Class;

   function Create (Success : Success_Pkg.Reference;
                    Failure : Failure_Pkg.Reference)
   return Callback_Pair.Object;

   overriding
   procedure Initialize (This : in out Callback_Pair.Object);

   overriding
   procedure Adjust (This : in out Callback_Pair.Object);

   overriding
   procedure Finalize (This : in out Callback_Pair.Object);

end Shared.Callback_Pair;
