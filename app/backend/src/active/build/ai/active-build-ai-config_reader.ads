------------------------------------------------------------------------------
-- @author  <gianmarco.midena@gmail.com>
-- @author  <stefanomunari.sm@gmail.com>
-- @author  <valle.sebastiano93@gmail.com>
-- @context application-backend::active-build-ai-config_reader
-- @purpose Reads and initializes the AI service
-- @interface Read_Config (Object, String):
--              starts the initialization process, given the name of the input
--              file
-- @dependencies -
-- @details -
------------------------------------------------------------------------------

package Active.Build.AI.Config_Reader is

   type Object is tagged null record;
   type Reference is access all Object'Class;

   procedure Read_Config (This      : in    Config_Reader.Object;
                          File_Name : in    String);

end Active.Build.AI.Config_Reader;
