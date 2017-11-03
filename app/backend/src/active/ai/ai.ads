------------------------------------------------------------------------------
-- @author  <gianmarco.midena@gmail.com>
-- @author  <stefanomunari.sm@gmail.com>
-- @author  <valle.sebastiano93@gmail.com>
-- @context application-backend::ai
-- @purpose This is an interface for accessing the AI service.
-- @interface Set_Clients_Limit (Object, Natural):
--              set the maximum number of travellers in a city
--            Init (Object, Natural, String, String, String, String):
--              initialize the AI service with the clients limit, the paths of
--              several resources used to compute the paths and a
--              <type, traveller_id> pair for a single traveller.
--            Find_Path (Object, String, String, Natural, String, String) -> T:
--              returns the list of steps which a traveller has to perform to
--              move from a source position to the intended destination
-- @dependencies -
-- @details This interface has an elevated number of parameters due to the fact
--          due to the fact that it represents a C-like interface
------------------------------------------------------------------------------
-- core
with Ada.Environment_Variables;
with Ada.Containers.Indefinite_Doubly_Linked_Lists;

package AI is

   package EV renames Ada.Environment_Variables;
   use Ada.Containers;

   package Step_List is
      new Ada.Containers.Indefinite_Doubly_Linked_Lists (
        Element_Type => String,
        "=" => "=");

   type Object is interface;
   type Reference is access all AI.Object'Class;

   not overriding
   procedure Set_Clients_Limit (
            This                : in AI.Object;
            New_Clients_Limit   :    Natural)
   is abstract;

   not overriding
   procedure Init (This           : in out AI.Object;
                   Clients_Limit  :        Natural;
                   Data_Path      :        String;
                   File_Prefix    :        String;
                   File_Extension :        String;
                   Agent_Id       :        String)
   is abstract;

   not overriding
   function Find_Path (
            This        : in AI.Object;
            Source      :    String;
            Destination :    String;
            Algorithm   :    Natural;
            S_Type      : in String;
            Agent_Id    : in String)
   return Step_List.List
   is abstract;

   not overriding
   procedure Finalize (This : in out AI.Object)
   is abstract;

-- City identifier
   City_Id         : String := EV.Value (Name => "CITY_NODE_ID");
   Config_File     : constant String := City_Id & "/ai" & City_Id & ".conf";
   Data_Path_Field : constant String := "path";
   File_Ext_Field  : constant String := "ext";
   Agents_No_Field : constant String := "agents_no";

private

  Clients_Limit : Natural := 0;

end AI;
