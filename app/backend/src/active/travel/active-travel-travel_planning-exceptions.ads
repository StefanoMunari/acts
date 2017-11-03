------------------------------------------------------------------------------
-- @author  <gianmarco.midena@gmail.com>
-- @author  <stefanomunari.sm@gmail.com>
-- @author  <valle.sebastiano93@gmail.com>
-- @context application-backend::active-travel-travel_planning-exceptions
-- @purpose Define procedures to raise exceptions related to the travel
--          planning phase
-- @interface Raise_Isolated_Street_Exceptiono
--              @deprecated
--            Raise_Dead_Street_Exception
--              @deprecated
--            Raise_Segmented_District_Exception
--              @deprecated
--            Raise_Wrong_Intersection_Ways_Number_Exception
--              @deprecated
--            Raise_Empty_SubSlice
--              raise an exception if the sub-slice has no stretches
-- @dependencies -
-- @details -
------------------------------------------------------------------------------

package Active.Travel.Travel_Planning.Exceptions is

   Isolated_Street : exception;
   Dead_Street : exception;
   Segmented_District : exception;
   SubSlice : exception;

   procedure Raise_Isolated_Street_Exception (Street_Id : Infra_Id);

   procedure Raise_Dead_Street_Exception (Street_Id : Infra_Id);

   procedure Raise_Segmented_District_Exception (
      Route_Source_Id, Route_Destination_Id : Infra_Id);

   procedure Raise_Wrong_Intersection_Ways_Number_Exception (
      Intersection_Id : in Infra_Id);

   procedure Raise_Empty_SubSlice (
      SubSliceName : in String);

end Active.Travel.Travel_Planning.Exceptions;
