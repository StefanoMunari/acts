with Active.Travel.Exceptions;
with Active.Travel.Travel_Planning;
with Active.Traveller.Utils;

with AI;

with Shared.Reader;
with Shared.Reader.JSON;
-- core
with Ada.Environment_Variables;
-- gnatcoll libs
with GNATCOLL.JSON;
-- core
with Ada.Unchecked_Deallocation;

package body Active.Travel is

   use Reactive.Stretch_Type_Package;
   use Slice;

   package Exc renames Active.Travel.Exceptions;
   package EV renames Ada.Environment_Variables;
   package G_JSON        renames GNATCOLL.JSON;

   function Create (
      Route_Source      : in     Slice.Map;
      Route_Destination : in     Slice.Map;
      Traveller_Id      : in     Agent.Agent_Id;
      Travel_State      : access Travel.Travel_State.Object'Class := null;
      Traveller_Utils   : access Active.Traveller.Utils.Object'Class := null)
   return Travel.Reference
   is
      Travel : Active.Travel.Reference := new Active.Travel.Object;
   begin
      Init (Travel, Route_Source, Route_Destination, Traveller_Id,
            Travel_State, Traveller_Utils);

      return Travel;
   end Create;

   procedure Init (
      This              : in out Travel.Reference;
      Route_Source      : in Slice.Map;
      Route_Destination : in Slice.Map;
      Traveller_Id      : in Agent.Agent_Id;
      Travel_State      : access Travel.Travel_State.Object'Class := null;
      Traveller_Utils   : access Active.Traveller.Utils.Object'Class
        := null)
   is
   begin
      This.Route_Source := Route_Source;
      This.Route_Destination := Route_Destination;
      This.Traveller_Id := Traveller_Id;

      if Travel_State = null then
         This.Travel_State := Active.Travel.Travel_Planning.Get_Instance;
      else
         This.Travel_State := Travel_State;
      end if;

      This.Traveller_Utils := Traveller_Utils;
      if This.Traveller_Utils = null then
         This.Traveller_Utils := Active.Traveller.Utils.Get_Instance;
      end if;

   end Init;

   function Get_Route_Source (This : in Travel.Object) return Slice.Map is
   begin
      if This.Route_Source.Is_Empty then
         Exc.Raise_Missing_Route_Source (This.Traveller_Id);
      end if;
      return This.Route_Source;
   end Get_Route_Source;

   function Get_Route_Destination (This : in Travel.Object) return Slice.Map is
   begin
      if This.Route_Destination.Is_Empty then
         Exc.Raise_Missing_Route_Destination (This.Traveller_Id);
      end if;
      return This.Route_Destination;
   end Get_Route_Destination;

   procedure Reverse_Source_And_Destination (This : in out Travel.Object)
   is
      Old_Source      : Slice.Map := This.Route_Source;
      Old_Destination : Slice.Map := This.Route_Destination;
   begin
     This.Route_Source      := Old_Destination;
     This.Route_Destination := Old_Source;
   end Reverse_Source_And_Destination;

   function Get_Traveller_Id (This : in Travel.Object)
      return Agent.Agent_Id is
   begin
      return This.Traveller_Id;
   end Get_Traveller_Id;

   procedure Clear_Route (
      This  : in out Travel.Object;
      Clean : out Boolean) is
   begin
      This.Route.Clear;
      Clean := This.Route.Is_Empty;
   end Clear_Route;

   procedure Prepend_Step (
      This    : in out Travel.Object;
      Step_Id : in     Infra_Id) is
   begin
      This.Route.Prepend (Step_Id);
   end Prepend_Step;

   procedure Change_Travel_State (
      This         : in out Travel.Object;
      Travel_State : access Travel.Travel_State.Object'Class) is
   begin
      This.Travel_State := Travel_State;
   end Change_Travel_State;

   procedure Advance (This : in out Travel.Object) is
   begin
      This.Travel_State.Advance (Travel => This);
   end Advance;

   function Is_Progressing (This : in Travel.Object) return Boolean is
   begin
      return This.Travel_State.Is_Progressing (Travel => This);
   end Is_Progressing;

   function Has_Next_Step (This : in Travel.Object) return Boolean
   is
      Current_Step : Infra_Id_List.Cursor := This.Route.First;
   begin
      if not Infra_Id_List.Has_Element (Current_Step) then
         return False;
      end if;
      Infra_Id_List.Next (Current_Step);
      return Infra_Id_List.Has_Element (Current_Step);
   end Has_Next_Step;

   procedure Consume_Step (This : in out Travel.Object)
   is
   begin
      if Natural (This.Route.Length) > 0 then
         This.Previous_Step := This.Route.First_Element;
         This.Route.Delete_First;
    end if;
   end Consume_Step;

   function Get_Current_Step_Id (This : in Travel.Object) return Infra_Id
   is
   begin
      return This.Route.First_Element;
   end Get_Current_Step_Id;

   function Get_Next_Step_Id (This : Travel.Object) return Infra_Id
   is
      Infrastructure_Id : Infra_Id;
      Next_Step         : Infra_Id_List.Cursor :=
         Infra_Id_List.Next (This.Route.First);
   begin
      If Infra_Id_List.Has_Element (Next_Step) then
         Infrastructure_Id := Infra_Id_List.Element (Position => Next_Step);
      else
         --Infrastructure_Id := This.Route_Destination;
         Infrastructure_Id :=
            This.Traveller_Utils.Get_List_From_Slice (
               This.Traveller_Id, This.Route_Destination).First_Element;
      end if;

      return Infrastructure_Id;
   end Get_Next_Step_Id;

   function Get_Previous_Step_Id (This : in Travel.Object) return Infra_Id
   is
   begin
      return This.Previous_Step;
   end Get_Previous_Step_Id;

   function Get_First_Step_Id (This : Travel.Object) return Infra_Id
   is
      First_Step_Id : Infra_Id := This.Route.First_Element;
   begin
      return First_Step_Id;
   end Get_First_Step_Id;

   function Get_Last_Step_Id (This : Travel.Object) return Infra_Id
   is
      Last_Step_Id : Infra_Id := This.Route.Last_Element;
   begin
      return Last_Step_Id;
   end Get_Last_Step_Id;

   function Get_Route (This : in Travel.Object) return Infra_Id_List.List is
   begin
      return This.Route.Copy;
   end Get_Route;

   procedure Finalize (This : in out Travel.Object) is
   begin
      null;
   end Finalize;

   function Get_Residual_Route (This : Travel.Object)
   return Infra_Id_List.List is
      Residual : Infra_Id_List.List := Infra_Id_List.Empty_List;
   begin
      for Step of This.Route loop
         Residual.Append (Step);
      end loop;

     return Residual;
   end Get_Residual_Route;

   procedure Set_Residual_Route (
      This           : in out Travel.Object;
      Residual_Route : in     Infra_Id_List.List)
   is
   begin
      This.Route := Infra_Id_List.Empty_List;
      for Step of Residual_Route loop
         This.Route.Append (Step);
      end loop;
   end Set_Residual_Route;

   not overriding
   function Contains (This :    Travel.Object;
                      Step : in Infra_Id)
   return Boolean
   is (This.Route.Contains (Step));

   function Contains (This        : Travel.Object;
                      Steps_Slice : in Slice.Map)
   return Boolean is
      S_Type     : Stretch_Type;
      Steps_List : Infra_Id_List.List;
   begin
      for T in Stretch_Type'Range loop

         S_Type   := Stretch_Type (T);
         Steps_List := Steps_Slice.Element (S_Type);

         for Step_Id of Steps_List loop
            if This.Route.Contains (Step_Id) then
               return True;
            end if;
         end loop;

      end loop;

      if This.Route_Destination = Steps_Slice then
         return True;
      end if;

      return False;
   end Contains;

   procedure Modify_Beginning (
      This          : in out Travel.Object;
      New_Beginning : in     Infra_Id_List.List)
   is
      New_Beginning_Var : Infra_Id_List.List := New_Beginning;
   begin
      for Step of New_Beginning loop
         This.Route.Delete_First;
      end loop;

      New_Beginning_Var.Reverse_Elements;

      for Step of New_Beginning_Var loop
         This.Route.Prepend (Step);
      end loop;
   end Modify_Beginning;

   function Get_AI (This : in Travel.Object)
   return access AI.Object'Class
   is (AI_Interface);

   procedure Set_AI (AI_Interface_Ref : access AI.Object'Class) is
   begin
      AI_Interface := AI_Interface_Ref;
   end Set_AI;

   procedure Add_AI (This : in Travel.Object;
      Stretch : in STP.Stretch_Type)
   is
      File_Reader : Shared.Reader.Reference := new Shared.Reader.JSON.Object;
   begin
      declare
         File_Path : String :=
            (EV.Value (Name => "CITY_ROOT")) &
            "/etc/init/" & AI.Config_File;
         Data      : G_JSON.JSON_Value := File_Reader.Parse (File_Path);
         Data_Path : String := Data.Get (AI.Data_Path_Field);
         File_Ext  : String  := Data.Get (AI.File_Ext_Field);
         Agents_No : Natural := Data.Get (AI.Agents_No_Field);
         A_Id      : String := SU.To_String (This.Traveller_Id);
         File_Prefix : String := STP.Stretch_Type'Image (Stretch);
      begin
         AI_Interface.Init (Agents_No, Data_Path, File_Prefix, File_Ext, A_Id);
      end;
   end Add_AI;


   procedure Remove_AI is
      procedure Free is new
         Ada.Unchecked_Deallocation (AI.Object'Class, AI.Reference);
   begin
      if AI_Interface /= null then
         AI_Interface.Finalize;
         Free (AI_Interface);
      end if;
   end Remove_AI;

   function Dump_State (This : Travel.Object) return SU.Unbounded_String is
   begin
      return This.Travel_State.Dump;
   end Dump_State;

   procedure Init_Route (This : in out Travel.Object; Route : Infra_Id_List.List)
   is
   begin
      This.Route := Route;
   end Init_Route;

end Active.Travel;
