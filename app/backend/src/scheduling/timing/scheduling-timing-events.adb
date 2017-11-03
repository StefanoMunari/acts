package body Scheduling.Timing.Events is

   function Register (
      Action_Ref   : Active.Agent.Reference;
      Executor_Ref : Scheduling.Executor.Reference;
      Span         : Real_Time.Time_Span)
   return Scheduling.Timing.Events.Reference
   is
      This : Scheduling.Timing.Events.Reference
         := new Scheduling.Timing.Events.Object;
   begin
      This.Action_Ref := Action_Ref;
      This.Executor_Ref := Executor_Ref;
      Timing_Events.Set_Handler (
         Timing_Events.Timing_Event (This.all),
         Span,
         Handlers.Handle'Access);
      return This;
   end Register;

   function Register (
      Action_Ref    : Active.Agent.Reference;
      Executor_Ref  : Scheduling.Executor.Reference;
      Deferral_Time : Real_Time.Time)
   return Scheduling.Timing.Events.Reference
   is
      This : Scheduling.Timing.Events.Reference
         := new Scheduling.Timing.Events.Object;
   begin
      This.Action_Ref := Action_Ref;
      This.Executor_Ref := Executor_Ref;
      Timing_Events.Set_Handler (
         Timing_Events.Timing_Event (This.all),
         Deferral_Time,
         Handlers.Handle'Access);
      return This;
   end Register;

   protected body Handlers is

      procedure Handle (Event : in out Timing_Events.Timing_Event)
      is
         Action_Ref   : Active.Agent.Reference
            := Object  (Timing_Events.Timing_Event'Class(Event)).Action_Ref;
         Executor_Ref : Scheduling.Executor.Reference
            := Object (Timing_Events.Timing_Event'Class(Event)).Executor_Ref;
      begin
         Executor_Ref.Execute (Action_Ref);
      end Handle;

   end Handlers;

end Scheduling.Timing.Events;
