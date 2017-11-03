with Interface_Layer.Tables.Pending_Request;

package body Scheduling.Timing.Callbacks is

   package Pending_Request_Pkg renames Interface_Layer.Tables.Pending_Request;

   procedure Register (Request_Id : SU.Unbounded_String)
   is
      This : Scheduling.Timing.Callbacks.Reference
         := new Scheduling.Timing.Callbacks.Object;
      Span : Ada.Real_Time.Time_Span := Ada.Real_Time.Milliseconds (1000);
   begin
      This.Request_Id := Request_Id;
      Timing_Events.Set_Handler (
         Timing_Events.Timing_Event (This.all),
         Span,
         Handlers.Handle'Access);
   end Register;

   protected body Handlers is

      procedure Handle (Event : in out Timing_Events.Timing_Event)
      is
         Request_Id : SU.Unbounded_String
            := Object  (Timing_Events.Timing_Event'Class(Event)).Request_Id;
      begin
         if Pending_Request_Pkg.Table.Contains (SU.To_String (Request_Id))
         then
            -- Here we could do something
            Register (Request_Id);
         end if;
      end Handle;

   end Handlers;

end Scheduling.Timing.Callbacks;
