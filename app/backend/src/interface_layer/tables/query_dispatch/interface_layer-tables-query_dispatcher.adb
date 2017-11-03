with Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;

with Active.Agent;

with Interface_Layer.Wrappers.Application.Concrete_Factory;

with Reactive.Infrastructure.Building.Parking_Manager;

package body Interface_Layer.Tables.Query_Dispatcher is

   package SU    renames Ada.Strings.Unbounded;
   package Agent renames Active.Agent;

   function Get_Instance (
      Host_Utils      : access Building.Host.Utils.Object'Class := null;
      District        : access District_Pkg.Object'Class := null;
      Stretch_Utils   : access Stretch_Utils_Pkg.Object'Class := null;
      Wrapper_Factory :
         access App_Wrapper.Abstract_Factory.Object'Class := null
   )
   return Query_Dispatcher.Reference is
      This : Query_Dispatcher.Reference := new Query_Dispatcher.Object;
   begin
      This.Host_Utils := Host_Utils;
      if This.Host_Utils = null then
         This.Host_Utils := Building.Host.Utils.Get_Instance;
      end if;

      This.District := District;
      if This.District = null then
         This.District := District_Pkg.Get_Instance;
      end if;

      This.Stretch_Utils := Stretch_Utils;
      if This.Stretch_Utils = null then
         This.Stretch_Utils := Stretch_Utils_Pkg.Get_Instance;
      end if;

      This.Wrapper_Factory := Wrapper_Factory;
      if This.Wrapper_Factory = null then
         This.Wrapper_Factory := new App_Wrapper.Concrete_Factory.Object;
      end if;

      return This;
   end Get_Instance;

   function Dispatch (This  :    Query_Dispatcher.Object;
                      Query : in String)
   return App_Wrapper.Reference
   is
   procedure Free is new  Ada.Unchecked_Deallocation (
      Query_Decoder.Object'Class, Query_Decoder.Reference);
      Decoder        : Query_Decoder.Reference := Query_Decoder.Create (Query);
      Procedure_Name : String := Decoder.Decode_Name;
   begin

      if Procedure_Name = "Book_Parking" then
         return This.Book_Parking (Decoder);
      end if;

      if Procedure_Name = "Seek" then
         return This.Seek (Decoder);
      end if;

   -- Add here other procedure dispatches

   -- If the incoming query does not match anyone of the available ones, then
   -- return an empty wrapper
      Free (Decoder);
      return This.Wrapper_Factory.Create_Wrapper;
   end Dispatch;

-- private part
   function Book_Parking (This    : Query_Dispatcher.Object;
                          Decoder : Query_Decoder.Reference)
   return App_Wrapper.Reference
   is
      Host_Id         : Infra_Id;
      Vehicle_Id      : Agent.Agent_Id;
      Parking_Manager : Building.Parking_Manager.Reference;
      Booked          : Boolean := False;
      Wrapper         : App_Wrapper.Reference;
   begin
   -- Get Access id (first argument)
      Host_Id  := Decoder.Decode_Arg (1);
   -- Get Vehicle id (second argument)
      Vehicle_Id := Decoder.Decode_Arg (2);

   -- Get Parking_Manager from Host
      Parking_Manager := This.Host_Utils.Get_Parking (Host_Id);

   -- Call Book_Parking on Parking_Manager
      Booked := Parking_Manager.Book_Parking (Vehicle_Id);

      Wrapper := This.Wrapper_Factory.Create_Wrapper (Booked);

      return Wrapper;
   end Book_Parking;

   function Seek (This    : Query_Dispatcher.Object;
                  Decoder : Query_Decoder.Reference)
   return App_Wrapper.Reference
   is
      Entity_Id         : Integer;
      Infrastructure_Id : Infra_Id;
      Host_Id           : Infra_Id;
      Agent_Id          : Agent.Agent_Id;
      Wrapper           : App_Wrapper.Reference;
   begin
   -- Get Entity id (first argument)
      Entity_Id := Decoder.Decode_Arg (1);

      Wrapper := This.Wrapper_Factory.Create_Wrapper (False);

   -- Look for infrastructures with this id
      Infrastructure_Id := Infra_Id (Entity_Id);
      if This.District.Contains_Infrastructure (Infrastructure_Id) then
         Wrapper := This.Wrapper_Factory.Create_Wrapper (True);
      end if;

   -- Look for hosts with this id
      Host_Id := Infra_Id (Entity_Id);
      if This.District.Contains_Host (Host_Id) then
         Wrapper := This.Wrapper_Factory.Create_Wrapper (True);
      end if;

   -- Look for travellers with this id
      Agent_Id := Agent.Create_Id_From_Natural (Entity_Id);
      if This.District.Contains_Traveller (Agent_Id) then
         Wrapper := This.Wrapper_Factory.Create_Wrapper (True);
      end if;

   -- Look for traffic lights with this id
      if This.District.Contains_Traffic_Light (Agent_Id) then
         Wrapper := This.Wrapper_Factory.Create_Wrapper (True);
      end if;

      return Wrapper;
   end Seek;

end Interface_Layer.Tables.Query_Dispatcher;
