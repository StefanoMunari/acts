------------------------------------------------------------------------------
-- @author  <gianmarco.midena@gmail.com>
-- @author  <stefanomunari.sm@gmail.com>
-- @author  <valle.sebastiano93@gmail.com>
-- @context application-backend::scheduling-remote-callback-success-book_parking
-- @purpose Executes successful callback for remote parking booking
-- @interface @inherit
-- @dependencies application-backend::active-agent
--               application-backend::active-people_carrier-utils
--               application-backend::active-traveller-utils
--               application-backend::reactive-infrastructure-building-parking_manager-garage
-- @details Interface.
------------------------------------------------------------------------------

with Active.Agent;
with Active.People_Carrier.Utils;
with Active.Traveller.Utils;

with Interface_Layer.Remote.Stub;
with Interface_Layer.Wrappers.Application.Abstract_Factory;

with Reactive.District;
with Reactive.Infrastructure.Building.Parking_Manager.Garage;

package Scheduling.Remote.Callback.Success.Book_Parking is

   package Pc_Utils_Pkg        renames Active.People_Carrier.Utils;
   package Traveller_Utils_Pkg renames Active.Traveller.Utils;
   package Stub_Pkg            renames Interface_Layer.Remote.Stub;
   package App_Wrapper_Pkg     renames Interface_Layer.Wrappers.Application;
   package Wrapper_Fac_Pkg     renames App_Wrapper_Pkg.Abstract_Factory;
   package District_Pkg        renames Reactive.District;
   package Garage_Pkg
      renames Reactive.Infrastructure.Building.Parking_Manager.Garage;

   type Object is new Success.Object
   with private;
   type Reference is access all Book_Parking.Object'Class;

   function Create (
      Traveller       : in     Active.Agent.Agent_Id;
      Vehicle         : in     Active.Agent.Agent_Id;
      Garage          : access Garage_Pkg.Object'Class;
      PC_Utils        : access PC_Utils_Pkg.Object'Class := null;
      District        : access District_Pkg.Object'Class := null;
      Traveller_Utils : access Traveller_Utils_Pkg.Object'Class := null;
      Wrapper_Factory : access Wrapper_Fac_Pkg.Object'Class := null;
      Stub            : access Stub_Pkg.Object'Class := null)
   return Success.Reference;

   overriding
   procedure Execute (This : in Success.Book_Parking.Object);

private
   type Object is new Success.Object
   with record
      Traveller       : Active.Agent.Agent_Id;
      Vehicle         : Active.Agent.Agent_Id;
      Garage          : access Garage_Pkg.Object'Class := null;
      PC_Utils        : access PC_Utils_Pkg.Object'Class := null;
      District        : access District_Pkg.Object'Class := null;
      Traveller_Utils : access Traveller_Utils_Pkg.Object'Class := null;
      Wrapper_Factory : access Wrapper_Fac_Pkg.Object'Class := null;
      Stub            : access Stub_Pkg.Object'Class;
   end record;

end Scheduling.Remote.Callback.Success.Book_Parking;
