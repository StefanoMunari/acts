------------------------------------------------------------------------------
-- @author  <gianmarco.midena@gmail.com>
-- @author  <stefanomunari.sm@gmail.com>
-- @author  <valle.sebastiano93@gmail.com>
-- @context application-backend::active-travel-exceptions
-- @purpose Define procedures to raise exceptions related to travels
-- @interface Raise_Missing_Route_Destination
--              raise an exception when no travel destination is provided
--            Raise_Missing_Route_Source
--              raise an exception when no travel source is provided
-- @dependencies -
-- @details -
------------------------------------------------------------------------------

package Active.Travel.Exceptions is

   procedure Raise_Missing_Route_Destination (Traveller_Id : Agent.Agent_Id);

   procedure Raise_Missing_Route_Source (Traveller_Id : Agent.Agent_Id);

end Active.Travel.Exceptions;
