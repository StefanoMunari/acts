------------------------------------------------------------------------------
-- @author  <gianmarco.midena@gmail.com>
-- @author  <stefanomunari.sm@gmail.com>
-- @author  <valle.sebastiano93@gmail.com>
-- @context application-backend::active-space_master
-- @purpose Singleton which embodies the logic behind moving travellers at
--          relative speeds
-- @interface Defer (Object, Agent_Id, Boolean):
--              computes the delay, according to the fact that the agent will
--              retry the action very soon (boolean parameter set to true) or
--              not
--            Defer (Object, Agent_Id, Natural):
--              like the other Defer, but with an explicit delay given
-- @dependencies application-backend::active-agent
--               application-backend::reactive-district
-- @details Singleton.
------------------------------------------------------------------------------

with Active.Agent;

limited with Reactive.District;

package Active.Space_Master is

   package Agent renames Active.Agent;
   pragma Suppress (Elaboration_Check);

   type Object is tagged limited private;
   type Reference is access all Active.Space_Master.Object'Class;

   package Next_Action_Type is
      type Next_Action is (DEFER, DO_NOT_DEFER, RETRY);
   end Next_Action_Type;

   function Get_Instance (
      District_Ref : access Reactive.District.Object'Class := null)
    return Active.Space_Master.Reference;

   function Get_Max_Speed (This : in Space_Master.Object) return Float;
-- Set_Max_Speed is called at init time by Active.Build.Config_Reader
   procedure Set_Max_Speed (This               : in out Space_Master.Object;
                            Possible_Max_Speed : in     Natural);
   procedure Clear (This : in out Space_Master.Object);

   procedure Defer (This         : in Space_Master.Object;
                    Traveller_Id : in Agent.Agent_Id;
                    Retry_Action : in Boolean);

   procedure Defer (This         : in Space_Master.Object;
                    Traveller_Id : in Agent.Agent_Id;
                    Deferred_To  : in Float);

private

   type Object is tagged limited record
      Max_Speed_Among_All_Entities : Float := 0.0;
      District_Ref                 : access District.Object'Class := null;
   end record;

   Instance : access Active.Space_Master.Object'Class := null;

   function Get_Deferral (This  : in Space_Master.Object;
                          Speed : in Float) return Float;

end Active.Space_Master;
