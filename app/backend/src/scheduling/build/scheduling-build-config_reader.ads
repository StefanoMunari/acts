------------------------------------------------------------------------------
-- @author  <gianmarco.midena@gmail.com>
-- @author  <stefanomunari.sm@gmail.com>
-- @author  <valle.sebastiano93@gmail.com>
-- @context application-backend::scheduling-build-config_reader
-- @purpose Reads the configuration for the whole scheduling subsystem
-- @interface Get_Instance() -> Reference:
--              returns an instance of the scheduling configuration reader
--            Read_Config (Object, String) -> Boolean
--              reads the configuration for the scheduling subsystem. Returns
--              true iff there were no errors during the process
-- @dependencies application-backend::scheduling-build-agenda_config_reader
--               application-backend::shared-reader
-- @details -
------------------------------------------------------------------------------

-- core
with Ada.Finalization;
-- libraries
with GNATCOLL.JSON;

with Scheduling.Build.Agenda.Config_Reader;
with Shared.Reader;

package Scheduling.Build.Config_Reader is

   package G_JSON renames GNATCOLL.JSON;

   type Object is new Ada.Finalization.Controlled with private;
   type Reference is access all Object'Class;

   function Get_Instance (
      File_Reader         : access Shared.Reader.Object'Class := null;
      Agenda_Reader       :
         access Build.Agenda.Config_Reader.Object'Class := null)
   return Config_Reader.Reference;

   function Read_Config (This      : in out Config_Reader.Object;
                         File_Path : in     String) return Boolean;

private
   type Object is new Ada.Finalization.Controlled with record
      File_Reader         : access Shared.Reader.Object'Class := null;
      Agenda_Reader       :
         access Build.Agenda.Config_Reader.Object'Class := null;
   end record;

end Scheduling.Build.Config_Reader;
