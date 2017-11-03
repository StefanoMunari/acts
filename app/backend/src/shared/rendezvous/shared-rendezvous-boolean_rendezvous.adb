with System;
use System;

package body Shared.Rendezvous.Boolean_Rendezvous is

   protected body Blocking_Rendezvous is

      entry Wait (Answer : out Boolean)
         when Answer_Available is
      begin
         Answer := The_Answer;
      end Wait;

      procedure Provide_Answer (Provided_Answer : in Boolean) is
      begin
         The_Answer := Provided_Answer;
         Answer_Available := TRUE;
      end Provide_Answer;

   end Blocking_Rendezvous;

   function "=" (A, B : Reference) return Boolean is
   begin
      return A.all'Address = B.all'Address;
   end "=";

end Shared.Rendezvous.Boolean_Rendezvous;
