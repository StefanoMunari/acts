------------------------------------------------------------------------------
-- @author  <gianmarco.midena@gmail.com>
-- @author  <stefanomunari.sm@gmail.com>
-- @author  <valle.sebastiano93@gmail.com>
-- @context application-backend::scheduling-remote-callback-failure-book_parking
-- @purpose Executes callback for failed remote parking booking
-- @interface @inherit
-- @dependencies application-backend::active-agent
--               application-backend::active-traveller-utils
--               application-backend::reactive-infrastructure-building-parking_manager-garage
-- @details Interface.
------------------------------------------------------------------------------

with Active.Agent;
with Active.Traveller.Utils;

with Reactive.Infrastructure.Building.Parking_Manager.Garage;

package Scheduling.Remote.Callback.Failure.Book_Parking is

   package Traveller_Utils_Pkg renames Active.Traveller.Utils;
   package Garage_Pkg
      renames Reactive.Infrastructure.Building.Parking_Manager.Garage;

   type Object is new Failure.Object
   with private;
   type Reference is access all Book_Parking.Object'Class;

   function Create (
      Traveller        : in     Active.Agent.Agent_Id;
      Vehicle          : in     Active.Agent.Agent_Id;
      Garage           : access Garage_Pkg.Object'Class;
      Traveller_Utils  : access Traveller_Utils_Pkg.Object'Class := null)
   return Failure.Reference;

   overriding
   procedure Execute (This : in Failure.Book_Parking.Object);

private
   type Object is new Failure.Object
   with record
      Traveller       : Active.Agent.Agent_Id;
      Vehicle         : Active.Agent.Agent_Id;
      Garage          : access Garage_Pkg.Object'Class := null;
      Traveller_Utils : access Traveller_Utils_Pkg.Object'Class := null;
   end record;

end Scheduling.Remote.Callback.Failure.Book_Parking;
