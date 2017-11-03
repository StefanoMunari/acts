with Ada.Unchecked_Deallocation;

with Active.Traveller;

with Interface_Layer.Utils.Types;
with Interface_Layer.Wrappers.Application;
with Interface_Layer.Wrappers.Application.Concrete_Factory;

package body Scheduling.Remote.Callback.Success.Book_Parking is

   package IL_Types         renames Interface_Layer.Utils.Types;
   package Concrete_Fac_Pkg renames App_Wrapper_Pkg.Concrete_Factory;

   use IL_Types.Recipient_Type_Pkg;

   function Create (
      Traveller       : in     Active.Agent.Agent_Id;
      Vehicle         : in     Active.Agent.Agent_Id;
      Garage          : access Garage_Pkg.Object'Class;
      PC_Utils        : access PC_Utils_Pkg.Object'Class := null;
      District        : access District_Pkg.Object'Class := null;
      Traveller_Utils : access Traveller_Utils_Pkg.Object'Class := null;
      Wrapper_Factory : access Wrapper_Fac_Pkg.Object'Class := null;
      Stub            : access Stub_Pkg.Object'Class := null)
   return Success.Reference
   is
      Instance : Success.Book_Parking.Reference
         := new Success.Book_Parking.Object;
   begin
     Instance.Traveller := Traveller;
     Instance.Vehicle := Vehicle;
     Instance.Garage := Garage;

     Instance.PC_Utils := PC_Utils;
     if PC_Utils = null then
       Instance.PC_Utils := PC_Utils_Pkg.Get_Instance;
     end if;

     Instance.District := District;
     if District = null then
       Instance.District := District_Pkg.Get_Instance;
     end if;

     Instance.Traveller_Utils := Traveller_Utils;
     if Traveller_Utils = null then
       Instance.Traveller_Utils := Traveller_Utils_Pkg.Get_Instance;
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

     return Success.Reference (Instance);
   end Create;

   procedure Execute (This : in Success.Book_Parking.Object)
   is
      Left        : Boolean;
      Vehicle_Var : Active.Agent.Agent_Id := This.Vehicle;
      Vehicle_Ref : Active.Traveller.Reference;
      Boarded     : Boolean;
   begin
      Vehicle_Ref := This.District.Find_Traveller_By_Id (This.Vehicle);
   -- add it to leaving ones and remove vehicle from pending
      This.Garage.Put_Leaving_Vehicle (
         Vehicle_Id => This.Vehicle,
         Driver_Id  => This.Traveller);
      This.Garage.Remove_Pending_Vehicle (This.Vehicle, This.Traveller);
      This.PC_Utils.Board (This.Vehicle, This.Traveller, Boarded);

   -- Traveller is the driver, Vehicle the vehicle
      if This.PC_Utils.Is_Carrier_Full (This.Vehicle) then
      -- Remove vehicle from garage
         Left := This.Garage.Leave_Parking (This.Traveller, Vehicle_Var);
      -- Traveller is the driver!
         if Left then
            This.Traveller_Utils.Set_Current_Speed (
               This.Vehicle,
               This.Traveller_Utils.Get_Maximum_Speed (This.Vehicle));
            This.Traveller_Utils.Defer (Vehicle_Var, Retry_Action => False);
         -- DON'T SEND EXIT_BUILDING EVENT: IT WILL BE SENT WHEN EXECUTING
         --+ AGAIN
         end if;
      else
      -- someone can still board
         This.Traveller_Utils.Set_Current_Speed (
            This.Traveller,
            This.Traveller_Utils.Get_Maximum_Speed (This.Traveller));
         This.Traveller_Utils.Defer (This.Traveller, Retry_Action => True);
      end if;
   end Execute;

end Scheduling.Remote.Callback.Success.Book_Parking;
