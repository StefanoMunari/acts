
package body Scheduling.Command_Broker is

   protected body Controller_For_Workers is

      procedure Init(Maximum : in Integer) is
      begin
         Total_Workers := Maximum;
      end Init;

      procedure Worker_Done is
      begin
        Stopped_Workers := Stopped_Workers + 1;
      end Worker_Done;

      entry Wait_For_Workers
        when Stopped_Workers = Total_Workers is
      begin
        null;
      end Wait_For_Workers;

   end Controller_For_Workers;

end Scheduling.Command_Broker;
