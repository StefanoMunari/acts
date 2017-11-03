with Ada.Unchecked_Deallocation;

with Active.Agent;
with Active.Traveller;

with Interface_Layer.Utils.Types;
with Interface_Layer.Wrappers.Application;
with Interface_Layer.Wrappers.Application.Concrete_Factory;

with Reactive.District;

package body Reactive.Infrastructure.Building.Host.Utils is

   package IL_Types         renames Interface_Layer.Utils.Types;
   package Concrete_Fac_Pkg renames App_Wrapper_Pkg.Concrete_Factory;

   use IL_Types.Recipient_Type_Pkg;

   function Get_Instance (
      PC_Utils        : access People_Carrier.Utils.Object'Class := null;
      District_Ref    : access Reactive.District.Object'Class := null;
      Wrapper_Factory : access Wrapper_Fac_Pkg.Object'Class := null;
      Stub            : access Stub_Pkg.Object'Class := null)
   return Host.Utils.Reference is
   begin
      if Instance = null then
         Instance := new Host.Utils.Object;
      end if;

      if PC_Utils = null then
         Instance.PC_Utils := People_Carrier.Utils.Get_Instance;
      else
         Instance.PC_Utils := PC_Utils;
      end if;

      if District_Ref = null then
         Instance.District_Ref := Reactive.District.Get_Instance;
      else
         Instance.District_Ref := District_Ref;
      end if;

      if Wrapper_Factory = null then
         Instance.Wrapper_Factory := new Concrete_Fac_Pkg.Object;
      else
         Instance.Wrapper_Factory := Wrapper_Factory;
      end if;

      if Stub = null then
         Instance.Stub := Stub_Pkg.Create;
      else
         Instance.Stub := Stub;
      end if;

      return Instance;
   end Get_Instance;

   function Get_Parking (This    : in out Host.Utils.Object;
                         Host_Id : in     Infra_Id)
   return Parking_Manager.Reference
   is
      Host_Ref : Host.Reference :=
         Host.Reference (This.District_Ref.Find_Host_By_Id (Host_Id));
   begin
   -- forward call to actual host reference
      return Host_Ref.Get_Parking;
   end Get_Parking;

   function Stop_Over (This         : in out Host.Utils.Object;
                       Host_Id      : in     Infra_Id;
                       Traveller_Id : in     Agent.Agent_Id)
   return Agent_Id_List.List
   is
      procedure Free is new  Ada.Unchecked_Deallocation (
         App_Wrapper_Pkg.Object'Class, App_Wrapper_Pkg.Reference);
      Traveller_Ref       : Active.Traveller.Reference
         := This.District_Ref.Find_Traveller_By_Id (Traveller_Id);
      Recipient           : Recipient_Type;
      T_Wrapper           : App_Wrapper_Pkg.Reference; -- Traveller wrapper
      Stopping_Travellers : Agent_Id_List.List;
      Boolean_Answer      : Boolean;
      Host_Ref            : Host.Reference :=
         Host.Reference (This.District_Ref.Find_Host_By_Id (Host_Id));
   begin
      T_Wrapper := This.Wrapper_Factory.Create_Wrapper (Traveller_Ref);
      Recipient.Id   := Host_Id;
      Recipient.Sort := IL_Types.HOST;
      This.Stub.Async_Request (
         T_Wrapper, IL_Types.ENTER_BUILDING, Recipient, Traveller_Id);
      Free (T_Wrapper);

      Stopping_Travellers.Append (Traveller_Id);
      if This.PC_Utils.Is_A_People_Carrier (Traveller_Id) then
         Stopping_Travellers := This.PC_Utils.Get_Passengers (Traveller_Id);
         for Freed_Id of Stopping_Travellers loop
            This.PC_Utils.Free (Traveller_Id, Freed_Id, Boolean_Answer);
         end loop;
      end if;

      -- Call Stop_Over on facility
      Host_Ref.Stop_Over (Stopping_Travellers);

      return Stopping_Travellers;
   end Stop_Over;

   function Exit_Building (This         : in out Host.Utils.Object;
                           Host_Id      : in     Infra_Id;
                           Traveller_Id : in     Agent.Agent_Id;
                           Vehicle_Id   :    out Agent.Agent_Id)
   return Next_Action
   is
      Host_Ref : Host.Reference := This.District_Ref.Find_Host_By_Id (Host_Id);
   begin
   -- forward call to actual host reference
      return Host_Ref.Exit_Building (Traveller_Id, Vehicle_Id);
   end Exit_Building;

   procedure Accessible_By (
      This       : in out Host.Utils.Object;
      Host_Id    : in     Infra_Id;
      Stretch_Id : in     Infra_Id;
      Stretch_T  : in     Stretch_Type)
   is
      Host_Ref : Host.Reference;
   begin
      Host_Ref := This.District_Ref.Find_Host_By_Id (Host_Id);
      Host_Ref.Accessible_By (Stretch_Id, Stretch_T);
   end Accessible_By;

   not overriding
   function Dump (
      This    : Host.Utils.Object;
      Host_Id : Infra_Id)
   return G_JSON.JSON_Value
   is
      Host_Ref : Host.Reference := This.District_Ref.Find_Host_By_Id (Host_Id);
   begin
      return Host_Ref.Dump;
   end Dump;

end Reactive.Infrastructure.Building.Host.Utils;
