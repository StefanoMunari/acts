with Ada.Unchecked_Deallocation;
with Ada.Strings.Unbounded;

with Active.Agent;

with Interface_Layer.Utils.Types;
with Interface_Layer.Wrappers.Application;
with Interface_Layer.Wrappers.Application.Concrete_Factory;

with Reactive.Infrastructure.Building.Parking_Manager.Garage;
with Reactive.Infrastructure.Building.Host.Use_Carrier_Strategy.Random_Strategy;

with Scheduling.Remote.Callback.Failure;
with Scheduling.Remote.Callback.Failure.Book_Parking;
with Scheduling.Remote.Callback.Success;
with Scheduling.Remote.Callback.Success.Book_Parking;

with Shared.Callback_Pair;
with Shared.Infra_Id_List;

package body Reactive.Infrastructure.Building.Host.Facility is

   package SU renames Ada.Strings.Unbounded;
   package Types renames Interface_Layer.Utils.Types;
   package Random_Strategy
      renames
         Reactive.Infrastructure.Building.Host.Use_Carrier_Strategy.Random_Strategy;
   package Garage_Pkg
      renames Reactive.Infrastructure.Building.Parking_Manager.Garage;
   package Failure_Callback_Pkg renames Scheduling.Remote.Callback.Failure;
   package Success_Callback_Pkg renames Scheduling.Remote.Callback.Success;
   package Callback_Pair_Pkg
      renames Shared.Callback_Pair;
   package Infra_Id_List renames Shared.Infra_Id_List;

   use Types.Recipient_Type_Pkg;
   use Reactive.Stretch_Type_Package;
   use Infra_Id_List;

   function Create (
      Id                   : Infra_Id;
      Parking_Ref          : Parking_Manager.Reference;
      Host_Utils_Ref       : access Host.Utils.Object'Class := null;
      PC_Utils             : access People_Carrier_Utils.Object'Class := null;
      District             : access District_Pkg.Object'Class := null;
      Traveller_Utils      : access Traveller_Utils_Pkg.Object'Class := null;
      Use_Carrier_Strategy : access UC_Strategy.Object'Class := null;
      Stub            : access Remote_Stub.Object'Class := null;
      Query_Builder   : access Remote_Query_Builder.Object'Class := null;
      Wrapper_Factory :
         access App_Wrapper.Abstract_Factory.Object'Class := null)
   return Facility.Reference
   is
      This   : Facility.Reference := new Facility.Object;
      S_Type : Stretch_Type;
   begin
      This.Id := Id;
      This.Hosted_Travellers := new Facility.Hosted_Travellers;
      This.Parking := Parking_Ref;
      Parking_Ref.Set_Host_Id (Id);

      This.Stretches := Slice.Empty_Map;
      for T in Stretch_Type'Range loop
         S_Type   := Stretch_Type (T);
         This.Stretches.Include (S_Type, Infra_Id_List.Empty_List);
      end loop;

      This.Host_Utils_Ref := Host_Utils_Ref;
      if This.Host_Utils_Ref = null then
         This.Host_Utils_Ref := Host.Utils.Get_Instance;
      end if;

      This.PC_Utils := PC_Utils;
      if This.PC_Utils = null then
         This.PC_Utils := People_Carrier_Utils.Get_Instance;
      end if;

      This.District := District;
      if This.District = null then
         This.District := District_Pkg.Get_Instance;
      end if;

      This.Traveller_Utils := Traveller_Utils;
      if This.Traveller_Utils = null then
         This.Traveller_Utils := Traveller_Utils_Pkg.Get_Instance;
      end if;

      This.Use_Carrier_Strategy := Use_Carrier_Strategy;
      if This.Use_Carrier_Strategy = null then
         This.Use_Carrier_Strategy := new Random_Strategy.Object;
      end if;

      if Stub = null then
         This.Stub := Remote_Stub.Create;
      else
         This.Stub := Stub;
      end if;

      if Query_Builder = null then
         This.Query_Builder := Remote_Query_Builder.Create;
      else
         This.Query_Builder := Query_Builder;
      end if;

      if Wrapper_Factory = null then
         This.Wrapper_Factory
            := new App_Wrapper.Concrete_Factory.Object;
      else
         This.Wrapper_Factory := Wrapper_Factory;
      end if;

      return This;
   end Create;

   function Get_Id (This : in Facility.Object)
   return Infra_Id is (This.Id);

   function Get_Parking (This : in out Facility.Object)
   return Parking_Manager.Reference is (This.Parking);

   procedure Stop_Over (This       : in out Facility.Object;
                        Travellers : in     Agent_Id_List.List)
   is
   begin
      for Traveller of Travellers loop
      -- if traveller_id is a vehicle, then it should be put in the garage.
         if This.PC_Utils.Is_A_People_Carrier (Traveller) then
            This.Parking.Park_Vehicle (Traveller);
         else
            This.Hosted_Travellers.Add_Traveller (Traveller);
         end if;
      end loop;
   end Stop_Over;

   overriding
   function Exit_Building (This         : in out Facility.Object;
                           Traveller_Id : in     Agent.Agent_Id;
                           Exiting_Id   :    out Agent.Agent_Id)
   return Next_Action
   is
      procedure Free is new  Ada.Unchecked_Deallocation (
         App_Wrapper.Object'Class, App_Wrapper.Reference);
      Traveller_Found : Boolean;
      Boarded         : Boolean;
      Vehicle_Id      : Agent.Agent_Id;
      Left            : Boolean;
      T_Wrapper       : App_Wrapper.Reference; -- Traveller wrapper
      Recipient           : Recipient_Type;
   begin
      Exiting_Id := Traveller_Id;
      This.Hosted_Travellers.Remove_Traveller (Traveller_Id, Traveller_Found);
      if Traveller_Found then
      -- Exiting now from building
         if This.Use_Carrier_Strategy.Use_Carrier_Or_Not then
         -- Look for vehicle
            This.Parking.Ask_For_Vehicle (Traveller_Id, Vehicle_Id, Boarded);
            if Boarded then
               if This.Parking.Is_A_Driver (Traveller_Id) then
                  This.Try_To_Book_Parking (Vehicle_Id, Traveller_Id);
               end if;
               return DO_NOT_DEFER;
            else
               -- schedule new check for next tick
               return RETRY;
            end if;
         end if;
         -- Start travel by foot by simply returning True and without boarding
         -- any vehicle
         Exiting_Id := Traveller_Id;
      else
      -- In vehicle exiting garage: move out of building
      -- The traveller is the driver of a vehicle which is leaving the building
         Left := This.Parking.Leave_Parking (Traveller_Id, Exiting_Id);
      end if;

   -- The traveller is going out by foot OR he is the driver.
   -- Exiting_Id is set correctly
      T_Wrapper := This.Wrapper_Factory.Create_Wrapper (
         This.District.Find_Traveller_By_Id (Traveller_Id));
      Recipient.Id   := This.Id;
      Recipient.Sort := Types.HOST;
      This.Stub.Async_Request (
         T_Wrapper, Types.EXIT_BUILDING, Recipient, Exiting_Id);
      Free (T_Wrapper);
      return DEFER;

   end Exit_Building;

   procedure Put_Stopping_Traveller (This         : in out Facility.Object;
                                     Traveller_Id : in     Agent.Agent_Id) is
   begin
      This.Hosted_Travellers.Add_Traveller (Traveller_Id);
   end Put_Stopping_Traveller;

   procedure Accessible_By (
      This       : in out Facility.Object;
      Stretch_Id : in     Infra_Id;
      Stretch_T  : in     Stretch_Type)
   is
      Current_List : Infra_Id_List.List;
   begin
      Current_List := This.Stretches.Element (Stretch_T);
      Current_List.Append (Stretch_Id);
      This.Stretches.Include (Stretch_T, Current_List);
   end Accessible_By;

   function Dump (This : Facility.Object) return G_JSON.JSON_Value
   is
      JSON                 : G_JSON.JSON_Value := G_JSON.Create_Object;
      Travellers           : G_JSON.JSON_Array
         := This.Hosted_Travellers.Get_Travellers;
      Parking_Manager_JSON : G_JSON.JSON_Value;
   begin
      JSON.Set_Field (Id_Field, Integer (This.Get_Id));
      JSON.Set_Field (Guests_Field, Travellers);

      -- Dump parking manager
      Parking_Manager_JSON := This.Parking.Dump;
      JSON.Set_Field (Garage_Field, Parking_Manager_JSON);

      return JSON;
   end Dump;

   procedure Try_To_Book_Parking (
      This         : in out Facility.Object;
      Vehicle_Id   : in     Agent.Agent_Id;
      Traveller_Id : in     Agent.Agent_Id)
   is
      procedure Free is new  Ada.Unchecked_Deallocation (
         App_Wrapper.Object'Class, App_Wrapper.Reference);
      Destination_Slice : Slice.Map :=
         This.Traveller_Utils.Get_Travel_Destination (Traveller_Id);
      This_Garage    : access Garage_Pkg.Object'Class
         := Garage_Pkg.Reference (This.Parking);
      Query_SU       : SU.Unbounded_String;
      Wrapped_Query  : App_Wrapper.Reference;
      Recipient      : Recipient_Type;
   begin
   -- If traveller boarded a vehicle, try booking a spot in the destination
   -- garage
      Query_SU :=
         This.Query_Builder
            .With_Correlation_Id (Traveller_Id)
            .With_Name           ("Book_Parking")
            .With_Arg            (This.Id)
            .With_Arg            (Vehicle_Id)
            .Get_Result;
      Wrapped_Query := This.Wrapper_Factory.Create_Wrapper (Query_SU);

      Recipient.Id   := This.Id;
      Recipient.Sort := Types.HOST;

      declare
         Success : Success_Callback_Pkg.Reference
            := Success_Callback_Pkg.Book_Parking.Create (
               Traveller_Id, Vehicle_Id, This_Garage);
         Failure : Failure_Callback_Pkg.Reference
            := Failure_Callback_Pkg.Book_Parking.Create (
               Traveller_Id, Vehicle_Id, This_Garage);
         Pair    : Callback_Pair_Pkg.Object
            := Callback_Pair_Pkg.Create (Success, Failure);
      begin
         This.Stub.Query (Wrapped_Query, Pair, Recipient, Traveller_Id);
      end;

      Free (Wrapped_Query);
   end Try_To_Book_Parking;

   protected body Hosted_Travellers is

      procedure Add_Traveller (Traveller_Id : in Agent.Agent_Id) is
      begin
         Travellers.Include (Traveller_Id);
      end Add_Traveller;

      procedure Remove_Traveller (Traveller_Id  : in     Agent.Agent_Id;
                                  Found         :    out Boolean) is
      begin
         if Travellers.Contains (Traveller_Id) then
            Travellers.Delete (Traveller_Id);
            Found := True;
         else
            Found := False;
         end if;
      end Remove_Traveller;

      function Contains_Traveller (Traveller_Id : in Agent.Agent_Id)
      return Boolean is (Travellers.Contains (Traveller_Id));

      function Get_Travellers return G_JSON.JSON_Array
      is
         Traveller_JSON  : G_JSON.JSON_Value;
         Travellers_JSON : G_JSON.JSON_Array := G_JSON.Empty_Array;
      begin
         for Traveller of Travellers loop
            Traveller_JSON := G_JSON.Create (Traveller);
            G_JSON.Append (Travellers_JSON, Traveller_JSON);
         end loop;
         return Travellers_JSON;
      end Get_Travellers;

   end Hosted_Travellers;

end Reactive.Infrastructure.Building.Host.Facility;
