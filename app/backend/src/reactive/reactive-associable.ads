------------------------------------------------------------------------------
-- @author  <gianmarco.midena@gmail.com>
-- @author  <stefanomunari.sm@gmail.com>
-- @author  <valle.sebastiano93@gmail.com>
-- @context application-backend::active-agent
-- @purpose Interface for entities which could enable the treading of a piece
--          of infrastructure which may or may not be local
-- @interface Try_To_Tread_Infrastructure (Object, Agent_Id, Infra_Id)
--                                        -> Boolean
--              tries to tread a piece of infrastructure, returns outcome
-- @dependencies -
-- @details Interface. It has a poor name but we didn't come up with something
--          better (consider refactoring for it)
------------------------------------------------------------------------------

with Active.Agent;

package Reactive.Associable is

   package Agent renames Active.Agent;
   use Reactive.Infra_Id_Type;

   type Object is interface;
   type Reference is access all Associable.Object'Class;

   not overriding
   procedure Try_To_Tread_Infrastructure (
      This         : in out Associable.Object;
      Traveller_Id : in     Agent.Agent_Id;
      Treadable_Id : in     Infra_Id;
      Advanced     :    out Boolean) is abstract;

end Reactive.Associable;
