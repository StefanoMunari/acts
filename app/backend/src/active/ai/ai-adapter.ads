------------------------------------------------------------------------------
-- @author  <gianmarco.midena@gmail.com>
-- @author  <stefanomunari.sm@gmail.com>
-- @author  <valle.sebastiano93@gmail.com>
-- @context application-backend::ai-adapter
-- @purpose This an implementation of the AI interface
-- @interface (@inherit AI)
-- @dependencies -
-- @details In this hierarchy we used the Adapter pattern, in order to decouple
--          the access from the actual AI service and be able to mock it or --          easily substitute it
------------------------------------------------------------------------------

package AI.Adapter is

-- type Object is tagged null record;
   type Object is new AI.Object with null record;
   type Reference is access all AI.Adapter.Object'Class;

   overriding
   procedure Set_Clients_Limit (
            This                : in AI.Adapter.Object;
            New_Clients_Limit   :    Natural);

   overriding
   procedure Init (This           : in out AI.Adapter.Object;
                   Clients_Limit  :        Natural;
                   Data_Path      :        String;
                   File_Prefix    :        String;
                   File_Extension :        String;
                   Agent_Id       :        String);

   overriding
   function Find_Path (
            This          : in AI.Adapter.Object;
            Source        :    String;
            Destination   :    String;
            Algorithm     :    Natural;
            S_Type        : in String;
            Agent_Id      : in String)
   return AI.Step_List.List;

   overriding
   procedure Finalize (This : in out AI.Adapter.Object);

end AI.Adapter;
