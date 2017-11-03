------------------------------------------------------------------------------
-- @author  <gianmarco.midena@gmail.com>
-- @author  <stefanomunari.sm@gmail.com>
-- @author  <valle.sebastiano93@gmail.com>
-- @context application-backend::active.build
-- @purpose Enclosing package for the active builder macro-component
-- @interface -
-- @dependencies -
-- @details -
------------------------------------------------------------------------------

package Active.Build is

-- stretch types
   Foot_Type : constant String := "FOOT";
   Bike_Type : constant String := "BIKE";
   Road_Type : constant String := "ROAD";
-- travel states
   Planning_State  : constant String := "PLANNING";
   Progress_State  : constant String := "PROGRESS";
   Completed_State : constant String := "COMPLETED";
-- traveller types
   Pedestrian_Type    : constant String := "PEDESTRIAN";
   Bicycle_Type       : constant String := "BICYCLE";
   Bus_Type           : constant String := "BUS";
   Private_Motor_Type : constant String := "PVT";
-- pvt types
   Car_Pvt_Type        : constant String := "CAR";
   Motorcycle_Pvt_Type : constant String := "MOTORCYCLE";
   Sidecar_Pvt_Type    : constant String := "SIDECAR";

end Active.Build;
