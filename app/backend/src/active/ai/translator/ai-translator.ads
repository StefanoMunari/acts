------------------------------------------------------------------------------
-- @author  <gianmarco.midena@gmail.com>
-- @author  <stefanomunari.sm@gmail.com>
-- @author  <valle.sebastiano93@gmail.com>
-- @context application-backend::ai-translator
-- @purpose Helper package to generate topology and costs for the AI
-- @interface Translate_To_Infrastructure (String):
--              given the file name, generate the topology
--            Translate_To_Costs (String):
--              given the file name, generate the costs of the graph edges
-- @dependencies -
-- @details This class uses some Python scripts which are in the same folder as
--          this file
------------------------------------------------------------------------------

with Ada.Environment_Variables;

package AI.Translator is

   package EV renames Ada.Environment_Variables;

   procedure Translate_To_Infrastructure (Snapshot_Name : in String);
   procedure Translate_To_Costs          (Snapshot_Name : in String);

   private
      City_Id    : String := EV.Value (Name => "CITY_NODE_ID");
      Local_Path : String :=
         EV.Value (Name => "CITY_ROOT") & "/src/active/ai/translator/";
      Data_Path : String :=
         EV.Value (Name => "CITY_ROOT") & "/var/snapshot/" & City_Id & "/";
end AI.Translator;
