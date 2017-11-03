------------------------------------------------------------------------------
-- @author  <gianmarco.midena@gmail.com>
-- @author  <stefanomunari.sm@gmail.com>
-- @author  <valle.sebastiano93@gmail.com>
-- @context application-backend::active-traveller-strategy-simple
-- @purpose Implementation of Active.Traveller.Strategy
-- @interface Wait_For_Bus_Or_Not (Object, Agent_Id, Infra_Id, Bus_Sign)
--                                -> Boolean:
--              @inherit
-- @dependencies application-backend::passive-road_sign-bus_stop
-- @details Concrete implementation for Strategy pattern
------------------------------------------------------------------------------

with Active.Traveller.Utils;

package Active.Traveller.Strategy.Simple is

   type Object is new Strategy.Object with private;
   type Reference is access all Strategy.Simple.Object'Class;

   function Get_Instance (
      Traveller_Utils : access Traveller.Utils.Object'Class := null)
   return Strategy.Simple.Reference;

   overriding
   function Wait_For_Bus_Or_Not (
      This           : Strategy.Simple.Object;
      Pedestrian_Id  : Agent.Agent_Id;
      Current_Stretch : Infra_Id;
      Bus_Stop_Ref   : Road_Sign.Bus_Stop.Reference)
   return Boolean;

private
   type Object is new Strategy.Object
   with record
      Traveller_Utils : access Traveller.Utils.Object'Class;
   end record;

end Active.Traveller.Strategy.Simple;
