------------------------------------------------------------------------------
-- @author  <gianmarco.midena@gmail.com>
-- @author  <stefanomunari.sm@gmail.com>
-- @author  <valle.sebastiano93@gmail.com>
-- @context application-backend::active-agent
-- @purpose Interface for entities which act
-- @interface Create_Id_From_Natural (Integer) -> Agent_Id
--              creates an agent id starting from an integer (it does the
--              trimming)
--            Get_Id (Object) -> Agent_Id
--              returns the id of an agent
--            Act (Object)
--              causes the agent to act
-- @dependencies -
-- @details Interface
------------------------------------------------------------------------------

with Ada.Strings.Unbounded;

package Active.Agent is

   package SU renames Ada.Strings.Unbounded;

   type Object is interface;
   type Reference is access all Agent.Object'Class;

   subtype Agent_Id is SU.Unbounded_String;

   function Create_Id_From_Natural (Id : Integer) return Agent_Id;

   function "=" (A, B : Agent_Id) return Boolean;

   function "<" (A, B : Agent_Id) return Boolean;

   not overriding
   function Get_Id (This : in Agent.Object) return Agent_Id is abstract;

   not overriding
   procedure Act (This : in out Agent.Object) is abstract;

end Active.Agent;
