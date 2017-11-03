------------------------------------------------------------------------------
-- @author  <gianmarco.midena@gmail.com>
-- @author  <stefanomunari.sm@gmail.com>
-- @author  <valle.sebastiano93@gmail.com>
-- @context application-backend::passive-road_sign
-- @purpose Interface for road signs
-- @interface Apply (Agent_Id):
--              applies the road sign to the given traveller
-- @dependencies application-backend::active-agent
-- @details Interface
------------------------------------------------------------------------------

-- gnatcoll libs
with GNATCOLL.JSON;

with Active.Agent;

package Passive.Road_Sign is

   package G_JSON renames GNATCOLL.JSON;
   package Agent  renames Active.Agent;

   type Object is interface;
   type Reference is access all Road_Sign.Object'Class;

   not overriding
   procedure Apply (This       : in out Road_Sign.Object;
                    Traveller  : in     Agent.Agent_Id)
   is abstract;

   not overriding
   function Dump (This : Road_Sign.Object) return G_JSON.JSON_Value
   is abstract;

end Passive.Road_Sign;
