with Reactive.District;

with Scheduling.Scheduler;

package body Scheduling.Build.Agenda.Config_Reader is

   function Read (
      This : in Agenda.Config_Reader.Object;
      Json : in G_JSON.JSON_Value)
   return Boolean
   is
      Agenda   : Agenda_Pkg.Map;
      Epoch    : Real_Time.Time_Span;
      Result   : Boolean := True;
      District : Reactive.District.Reference := Reactive.District.Get_Instance;
   begin
      Agenda := This.Read_Agenda (Json);
      Epoch  := This.Read_Epoch (Json);

   -- Init the scheduler
      Scheduling.Scheduler.Instance.Start (
         Initial_Agenda => Agenda,
         District       => District,
         Epoch          => Epoch);

      return Result;
   end Read;

   function Read_Agenda (
      This        : in  Agenda.Config_Reader.Object;
      Agenda_Json : in  G_JSON.JSON_Value)
   return Agenda_Pkg.Map
   is
   begin
      return Json_To_Agenda (Agenda_Json);
   end Read_Agenda;

   function Read_Epoch (
      This : in Agenda.Config_Reader.Object;
      Json : in G_JSON.JSON_Value)
   return Real_Time.Time_Span
   is
      Epoch_Int : Integer;
   begin
      Epoch_Int := G_JSON.Get (Json, Elapsed_Field);
      return Real_Time.Milliseconds (Epoch_Int);
   end Read_Epoch;

end Scheduling.Build.Agenda.Config_Reader;