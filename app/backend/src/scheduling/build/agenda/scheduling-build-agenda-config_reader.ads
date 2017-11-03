------------------------------------------------------------------------------
-- @author  <gianmarco.midena@gmail.com>
-- @author  <stefanomunari.sm@gmail.com>
-- @author  <valle.sebastiano93@gmail.com>
-- @context application-backend::scheduling-build-agenda-config_reader
-- @purpose Reads the configuration for creating a single agenda
-- @interface Read (Object, JSON) -> Boolean
--              converts JSON data to a valid agenda
-- @dependencies -
-- @details -
------------------------------------------------------------------------------

-- core
with Ada.Finalization;

package Scheduling.Build.Agenda.Config_Reader is

   type Object is new Ada.Finalization.Controlled with null record;
   type Reference is access all Object'Class;

   not overriding
   function Read (
      This : in Agenda.Config_Reader.Object;
      Json : in G_JSON.JSON_Value)
   return Boolean;

private
   not overriding
   function Read_Agenda (
      This        : in Agenda.Config_Reader.Object;
      Agenda_Json : in G_JSON.JSON_Value)
   return Agenda_Pkg.Map;

   not overriding
   function Read_Epoch (
      This : in Agenda.Config_Reader.Object;
      Json : in G_JSON.JSON_Value)
   return Real_Time.Time_Span;

end Scheduling.Build.Agenda.Config_Reader;