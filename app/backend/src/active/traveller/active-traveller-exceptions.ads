------------------------------------------------------------------------------
-- @author  <gianmarco.midena@gmail.com>
-- @author  <stefanomunari.sm@gmail.com>
-- @author  <valle.sebastiano93@gmail.com>
-- @context application-backend::active-travel-exceptions
-- @purpose Define procedures to raise exceptions related to travels
-- @interface Raise_Null_Travel_Reference
--              raise an exception before throwing an NPE by trying to access
--              a null travel ref
--            Raise_Maximum_Speed_Exceeded_Exception
--              raise an exception when trying to set a velocity above the
--              maximum possible value for a given type of traveller
--            Raise_Unknown_Direction_Exception
--              @deprecated
-- @dependencies application-backend::active-agent
-- @details -
------------------------------------------------------------------------------

with Active.Agent;

package Active.Traveller.Exceptions is

   package Agent renames Active.Agent;

   procedure Raise_Null_Travel_Reference (
      Traveller_Id : Agent.Agent_Id);

   procedure Raise_Maximum_Speed_Exceeded_Exception (
      Traveller_Id : Agent.Agent_Id;
      Traveller_Maximum_Speed, New_Speed : Natural);

   procedure Raise_Unknown_Direction_Exception (
      Traveller_Id : Agent.Agent_Id);

end Active.Traveller.Exceptions;
