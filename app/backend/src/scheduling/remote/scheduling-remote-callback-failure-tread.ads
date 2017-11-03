------------------------------------------------------------------------------
-- @author  <gianmarco.midena@gmail.com>
-- @author  <stefanomunari.sm@gmail.com>
-- @author  <valle.sebastiano93@gmail.com>
-- @context application-backend::scheduling-remote-callback-failure-tread
-- @purpose Type which executes failure callback for Tread operations
-- @interface @inherit
-- @dependencies application-backend::active-agent
--               application-backend::active-traveller-utils
-- @details Interface.
------------------------------------------------------------------------------

with Active.Agent;
with Active.Traveller.Utils;

package Scheduling.Remote.Callback.Failure.Tread is

   package Traveller_Utils_Pkg renames Active.Traveller.Utils;

   type Object is new Failure.Object
   with private;
   type Reference is access all Tread.Object'Class;

   function Create (
      Traveller       : in     Active.Agent.Agent_Id;
      Traveller_Utils : access Traveller_Utils_Pkg.Object'Class := null)
   return Failure.Reference;

   overriding
   procedure Execute (This : in Failure.Tread.Object);

private
   type Object is new Failure.Object
   with record
      Traveller       : Active.Agent.Agent_Id;
      Traveller_Utils : access Traveller_Utils_Pkg.Object'Class := null;
   end record;

end Scheduling.Remote.Callback.Failure.Tread;
