------------------------------------------------------------------------------
-- @author  <gianmarco.midena@gmail.com>
-- @author  <stefanomunari.sm@gmail.com>
-- @author  <valle.sebastiano93@gmail.com>
-- @context application-backend::scheduling
-- @purpose Enclosing package for the scheduling sub-system
-- @interface Agenda_To_Json (Agenda, Time) -> JSON:
--              Dumps an agenda to JSON
--            Json_To_Agenda (JSON) -> Agenda:
--              Creates an agenda starting from JSON data
-- @dependencies application-backend::active-agent
-- @details It embodies the Agenda type, which maps agent ids to time instants
--          on which the correspondent agents will have to act
------------------------------------------------------------------------------

-- core
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Ordered_Maps;
with Ada.Real_Time;
-- gnatcoll libs
with GNATCOLL.JSON;

with Active.Agent;

package Scheduling is
   package Real_Time renames Ada.Real_Time;
   package G_JSON    renames GNATCOLL.JSON;

   package Agenda_Pkg is
      new Ada.Containers.Ordered_Maps (
         Key_Type        => Active.Agent.Agent_Id,
         Element_Type    => Real_Time.Time,
         "="             => Real_Time."=",
         "<"             => Active.Agent."<");

-- Provides conversion from Agenda_Pkg.Map to JSON
   function Agenda_To_Json (
      Agenda    : Agenda_Pkg.Map;
      Stop_Time : Real_Time.Time)
   return G_JSON.JSON_Value;

-- Provides conversion from JSON to Agenda_Pkg.Map
   function Json_To_Agenda (Agenda_JSON : in G_JSON.JSON_Value)
   return Agenda_Pkg.Map;

   Actions_Field  : constant String := "actions";
   Agent_Field    : constant String := "who";
   Time_Field     : constant String := "when";
   Elapsed_Field  : constant String := "elapsedTime";

end Scheduling;
