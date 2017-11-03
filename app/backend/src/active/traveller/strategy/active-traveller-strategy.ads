------------------------------------------------------------------------------
-- @author  <gianmarco.midena@gmail.com>
-- @author  <stefanomunari.sm@gmail.com>
-- @author  <valle.sebastiano93@gmail.com>
-- @context application-backend::active-traveller-strategy
-- @purpose Interface for strategies related to travellers
-- @interface Wait_For_Bus_Or_Not (Object, Agent_Id, Infra_Id, Bus_Sign)
--                                -> Boolean:
--              decides if a pedestrian has to wait for a bus
-- @dependencies application-backend::passive-road_sign-bus_stop
-- @details Strategy pattern
------------------------------------------------------------------------------

with Passive.Road_Sign.Bus_Stop;

package Active.Traveller.Strategy is

   package Road_Sign renames Passive.Road_Sign;

   type Object is interface;
   type Reference is access all Traveller.Strategy.Object'Class;

   not overriding
   function Wait_For_Bus_Or_Not (
      This            : Traveller.Strategy.Object;
      Pedestrian_Id   : Agent.Agent_Id;
      Current_Stretch : Infra_Id;
      Bus_Stop_Ref    : Road_Sign.Bus_Stop.Reference)
   return Boolean is abstract;

end Active.Traveller.Strategy;
