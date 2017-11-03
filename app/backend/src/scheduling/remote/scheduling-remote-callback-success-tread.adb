with Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;

with Active.Traveller;

with Interface_Layer.Utils.Types;
with Interface_Layer.Wrappers.Application.Concrete_Factory;

with Reactive.Treadable;

package body Scheduling.Remote.Callback.Success.Tread is

   package SU              renames Ada.Strings.Unbounded;
   package IL_Types        renames Interface_Layer.Utils.Types;
   package Con_Factory_Pkg renames App_Wrapper_Pkg.Concrete_Factory;
   use Reactive.Treadable;

   use IL_Types.Recipient_Type_Pkg;

   function Create (
      Traveller : in     Active.Agent.Agent_Id;
      Treadable : in     Infra_Id;
      District  : access District_Pkg.Object'Class := null;
      PC_Utils  : access PC_Utils_Pkg.Object'Class := null;
      W_Factory : access Abs_Factory_Pkg.Object'Class:= null;
      Stub      : access Stub_Pkg.Object'Class := null)
   return Success.Reference
   is
      Instance : Success.Tread.Reference := new Success.Tread.Object;
   begin
      Instance.Traveller := Traveller;
      Instance.Treadable := Treadable;

      if District = null then
         Instance.District := District_Pkg.Get_Instance;
      else
         Instance.District := District;
      end if;

      if PC_Utils = null then
         Instance.PC_Utils := PC_Utils_Pkg.Get_Instance;
      else
         Instance.PC_Utils := PC_Utils;
      end if;

      if W_Factory = null then
         Instance.W_Factory := new Con_Factory_Pkg.Object;
      else
         Instance.W_Factory := W_Factory;
      end if;

      if Stub = null then
         Instance.Stub := Stub_Pkg.Create;
      else
         Instance.Stub := Stub;
      end if;

      return Success.Reference (Instance);
   end Create;

   procedure Execute (This : in Success.Tread.Object)
   is
      procedure Free is new  Ada.Unchecked_Deallocation (
         App_Wrapper_Pkg.Object'Class, App_Wrapper_Pkg.Reference);
      Traveller_Ref    : Active.Traveller.Reference;
      T_Wrapper        : App_Wrapper_Pkg.Reference; -- Traveller wrapper
      Recipient        : Recipient_Type;
      Old_Position     : Infra_Id;
      Old_Position_Ref : Reactive.Treadable.Reference;
      Left             : Boolean;
      Removed          : Boolean;
   begin
      if This.District.Contains_Traveller (This.Traveller) then
         Traveller_Ref  := This.District.Find_Traveller_By_Id (This.Traveller);

      -- Leave last treadable
         Old_Position := Traveller_Ref.Get_Position;
         Old_Position_Ref :=
            This.District.Find_Treadable_By_Id (Old_Position);
         if Old_Position_Ref /= null then
            Old_Position_Ref.Leave (This.Traveller, Left);
         end if;

      -- Send event notification
         T_Wrapper      := This.W_Factory.Create_Wrapper (Traveller_Ref);
         Recipient.Id   := This.Treadable;
         Recipient.Sort := IL_Types.TREADABLE;
         This.Stub.Async_Request (
            T_Wrapper, IL_Types.TREAD, Recipient, This.Traveller);
         Free (T_Wrapper);
      end if;

      if This.PC_Utils.Is_A_People_Carrier (This.Traveller) then
         for Passenger of This.PC_Utils.Get_Passengers (This.Traveller) loop
            if This.District.Contains_Traveller (Passenger) then
               This.District.Remove_Traveller (Passenger, Removed);
            end if;
         end loop;
      end if;

      if This.District.Contains_Traveller (This.Traveller) then
         This.District.Remove_Traveller (This.Traveller, Removed);
      end if;

   end Execute;

end Scheduling.Remote.Callback.Success.Tread;
