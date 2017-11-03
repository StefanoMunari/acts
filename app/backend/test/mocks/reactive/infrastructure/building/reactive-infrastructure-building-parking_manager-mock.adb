with Mock.Exceptions;

use Mock.Exceptions;

package body Reactive.Infrastructure.Building.Parking_Manager.Mock is

   function Create return Parking_Manager.Reference
   is (new Parking_Manager.Mock.Object);

   procedure Park_Vehicle (This         : in out Parking_Manager.Mock.Object;
                           Traveller_Id : in     Agent.Agent_Id) is
   begin
      This.Mock_Values.Park_Vehicle_Called := True;
   end Park_Vehicle;

   procedure Ask_For_Vehicle (This          : in out Mock.Object;
                              Traveller_Id  : in      Agent.Agent_Id;
                              Vehicle_Id    :    out Agent.Agent_Id;
                              Vehicle_Found :    out Boolean)
   is
   begin
      if not This.Return_Values.Vehicle_Found_Existence then
         Raise_Missing_Return_Value_For_Mocked_Procedure_Exception
           (Parameter_Name => "Returned values",
            Procedure_Name => "Ask_For_Vehicle",
            Package_Name => "Reactive.Infrastructure.Stretch"
                            & ".Decoration.Parking_Manager.Mock");
      end if;

      Vehicle_Id    := This.Return_Values.Vehicle_Id;
      Vehicle_Found := This.Return_Values.Vehicle_Found;
   end Ask_For_Vehicle;

   function Leave_Parking (This         : in out Parking_Manager.Mock.Object;
                           Passenger_Id : in     Agent.Agent_Id;
                           Vehicle_Id   :    out Agent.Agent_Id)
   return Boolean is
   begin
      if not This.Return_Values.Leave_Parking_Existence then
         Raise_Missing_Return_Value_For_Mocked_Procedure_Exception
           (Parameter_Name => "Returned values",
            Procedure_Name => "Leave_Parking",
            Package_Name => "Reactive.Infrastructure.Stretch"
                            & ".Decoration.Parking_Manager.Mock");
      end if;

      Vehicle_Id := This.Return_Values.Leave_Parking_Id;
      return This.Return_Values.Leave_Parking_Bool;
   end Leave_Parking;

   function Book_Parking (This       : in out Parking_Manager.Mock.Object;
                          Vehicle_Id : in     Agent.Agent_Id)
   return Boolean is
   begin
      if not This.Return_Values.Book_Parking_Existence then
         Raise_Missing_Return_Value_For_Mocked_Procedure_Exception (
            Parameter_Name => "Returned values",
            Procedure_Name => "Book_Parking",
            Package_Name   => "Reactive.Infrastructure.Building"
                            & ".Parking_Manager.Mock");
      end if;

      -- TODO: Implement check on vehicle id
      -- Vehicle_Id := This.Return_Values.Book_Parking_Id;
      return This.Return_Values.Book_Parking_Bool;
   end Book_Parking;

   overriding
   procedure Set_Host_Id (This    : in out Parking_Manager.Mock.Object;
                          Host_Id : in     Infra_Id) is
   begin
      This.Mock_Values.Set_Host_Id_Value := Host_Id;
   end Set_Host_Id;

   procedure Set_Size (This : in out Parking_Manager.Mock.Object;
                       Size : in     Natural) is
   begin
      This.Mock_Values.Set_Size_Value := Size;
   end Set_Size;

   function Dump (This : Parking_Manager.Mock.Object) return G_JSON.JSON_Value
   is
   begin
      if not This.Return_Values.Dump_Existence then
         Raise_Missing_Return_Value_For_Mocked_Procedure_Exception (
            Parameter_Name => "Returned values",
            Procedure_Name => "Dump",
            Package_Name   => "Reactive.Infrastructure.Building"
                            & ".Parking_Manager.Mock");
      end if;

      return This.Return_Values.Dump;
   end Dump;

   procedure Set_Values_For_Ask_For_Vehicle (
      This          : in out Parking_Manager.Mock.Object;
      Vehicle_Id    : in     Agent.Agent_Id;
      Vehicle_Found : in     Boolean) is
   begin
      This.Return_Values.Vehicle_Id := Vehicle_Id;
      This.Return_Values.Vehicle_Id_Existence := True;
      This.Return_Values.Vehicle_Found := Vehicle_Found;
      This.Return_Values.Vehicle_Found_Existence := True;
   end Set_Values_For_Ask_For_Vehicle;

   procedure Set_Values_For_Leave_Parking (
      This          : in out Parking_Manager.Mock.Object;
      Leaving_Id    : in     Agent.Agent_Id;
      Is_Leaving    : in     Boolean) is
   begin
      This.Return_Values.Leave_Parking_Id        := Leaving_Id;
      This.Return_Values.Leave_Parking_Bool      := Is_Leaving;
      This.Return_Values.Leave_Parking_Existence := True;
   end Set_Values_For_Leave_Parking;

   procedure Set_Values_For_Book_Parking (
      This       : in out Parking_Manager.Mock.Object;
      Booking_Id : in     Agent.Agent_Id;
      Is_Booking : in     Boolean) is
   begin
      This.Return_Values.Book_Parking_Id        := Booking_Id;
      This.Return_Values.Book_Parking_Bool      := Is_Booking;
      This.Return_Values.Book_Parking_Existence := True;
   end Set_Values_For_Book_Parking;

   procedure Set_Return_Value_For_Dump (
      This         : in out Parking_Manager.Mock.Object;
      Return_Value : in     G_JSON.JSON_Value) is
   begin
      This.Return_Values.Dump := Return_Value;
      This.Return_Values.Dump_Existence := True;
   end Set_Return_Value_For_Dump;

   function Get_Park_Vehicle_Called (
      This   : in out Parking_Manager.Mock.Object) return Boolean is
   begin
      return This.Mock_Values.Park_Vehicle_Called;
   end Get_Park_Vehicle_Called;

   function Get_Set_Host_Id (
      This : in out Parking_Manager.Mock.Object) return Infra_Id
   is (This.Mock_Values.Set_Host_Id_Value);

   function Get_Set_Size (
      This : in out Parking_Manager.Mock.Object) return Natural
   is (This.Mock_Values.Set_Size_Value);

end Reactive.Infrastructure.Building.Parking_Manager.Mock;
