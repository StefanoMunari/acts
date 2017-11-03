with Ada.Strings.Unbounded;

package body Scheduling is

   package SU renames Ada.Strings.Unbounded;

   use Real_Time;

   function Agenda_To_Json (
      Agenda    : Agenda_Pkg.Map;
      Stop_Time : Real_Time.Time)
   return G_JSON.JSON_Value
   is
      Actions_JSON  : G_JSON.JSON_Array := G_JSON.Empty_Array;
      Action_JSON   : G_JSON.JSON_Value := G_JSON.Create_Object;
      Who_Act       : Active.Agent.Agent_Id;
      When_Act      : Real_Time.Time;
      Actual_When   : Real_Time.Time;
      SC            : Real_Time.Seconds_Count;
      TS            : Real_Time.Time_Span;
      When_Seconds  : Integer;
   begin
      for Action in Agenda.Iterate loop
         Who_Act := Agenda_Pkg.Key(Action);
         When_Act := Agenda_Pkg.Element(Action);

         Action_JSON := G_JSON.Create_Object;

         Action_JSON.Set_Field (Agent_Field, SU.Unbounded_String (Who_Act));

         Actual_When := Real_Time.Time_Of (0, When_Act - Stop_Time);
         Real_Time.Split (Actual_When, SC, TS);
         When_Seconds := Integer (Float (SC) * 1000.0);
         Action_JSON.Set_Field (Time_Field, When_Seconds);

         G_JSON.Append (Actions_JSON, Action_JSON);
      end loop;
      return G_JSON.Create (Actions_JSON);
   end Agenda_To_Json;

   function Json_To_Agenda (Agenda_JSON : in G_JSON.JSON_Value)
   return Agenda_Pkg.Map
   is
      Agenda          : Agenda_Pkg.Map := Agenda_Pkg.Empty_Map;
      Actions_JSON    : G_JSON.JSON_Array;
      Action_JSON     : G_JSON.JSON_Value := G_JSON.Create_Object;
      Action_Id_Int   : Integer;
      Action_Id       : Active.Agent.Agent_Id;
      Action_Time_Int : Natural;
      Action_Time     : Real_Time.Time;
      Actions_L       : Integer;
   begin
      Actions_JSON := Agenda_JSON.Get (Actions_Field);
      Actions_L := G_JSON.Length (Actions_JSON);

      for I in 1 .. Actions_L loop
         Action_JSON := G_JSON.Get (Actions_JSON, I);

         Action_Id_Int := Action_JSON.Get (Agent_Field);
         Action_Id     := Active.Agent.Create_Id_From_Natural (Action_Id_Int);

         -- Get time for action
         Action_Time_Int := G_JSON.Get (Actions_JSON, I).Get (Time_Field);

         -- Add entry to agenda map
         Action_Time :=
            Real_Time.Clock + Real_Time.Milliseconds (Action_Time_Int);
         Agenda.Include (Action_Id, Action_Time);
      end loop;

      return Agenda;
   end Json_To_Agenda;

end Scheduling;
