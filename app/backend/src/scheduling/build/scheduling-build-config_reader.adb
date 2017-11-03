with Scheduling.Build.Exceptions;
use Scheduling.Build.Exceptions;

with Shared.Reader.JSON;

package body Scheduling.Build.Config_Reader is

   function Get_Instance (
      File_Reader         : access Shared.Reader.Object'Class := null;
      Agenda_Reader       :
         access Build.Agenda.Config_Reader.Object'Class := null)
   return Config_Reader.Reference
   is
      Instance : Config_Reader.Reference := new Config_Reader.Object;
   begin

      Instance.File_Reader := File_Reader;
      if Instance.File_Reader = null then
         Instance.File_Reader := new Shared.Reader.JSON.Object;
      end if;

      Instance.Agenda_Reader := Agenda_Reader;
      if Instance.Agenda_Reader = null then
         Instance.Agenda_Reader := new Build.Agenda.Config_Reader.Object;
      end if;

      return Instance;
   end Get_Instance;

   function Read_Config (This      : in out Config_Reader.Object;
                         File_Path : in     String)
   return Boolean
   is
      Scheduling_Value : G_JSON.JSON_Value
         := This.File_Reader.Parse (File_Path);
   begin
    return This.Agenda_Reader.Read (Scheduling_Value);
  end Read_Config;

end Scheduling.Build.Config_Reader;
