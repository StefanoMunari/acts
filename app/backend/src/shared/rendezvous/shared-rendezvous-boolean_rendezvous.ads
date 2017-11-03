------------------------------------------------------------------------------
-- @author  <gianmarco.midena@gmail.com>
-- @author  <stefanomunari.sm@gmail.com>
-- @author  <valle.sebastiano93@gmail.com>
-- @context application-backend::shared-rendezvous-boolean_rendezvous
-- @purpose Protected Object which exposes
--           1) a blocking entry on which wait for a boolean answer
--           2) a procedure to provide the answer
-- @interface Wait -> Boolean:
--              blocks the requesting task on an entry, waiting for the answer
--              to be provided
--            Provide_Answer (Boolean)
--              causes the entry guard to trigger, providing the answer
-- @dependencies -
-- @details Protected Object (=> thread safe)
------------------------------------------------------------------------------

package Shared.Rendezvous.Boolean_Rendezvous is

   protected type Blocking_Rendezvous is

      entry Wait (Answer : out Boolean);

      procedure Provide_Answer (Provided_Answer : in Boolean);
   private
      The_Answer       : Boolean;
      Answer_Available : Boolean := False;
   end Blocking_Rendezvous;
   type Reference is access all Blocking_Rendezvous;

   function "=" (A, B : Reference) return Boolean;

end Shared.Rendezvous.Boolean_Rendezvous;
