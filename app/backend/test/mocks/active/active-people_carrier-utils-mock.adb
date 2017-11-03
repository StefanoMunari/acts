with Ada.Strings.Unbounded;

with Active.Agent;
with Active.People_Carrier;

with Mock.Exceptions;
use Mock.Exceptions;

package body Active.People_Carrier.Utils.Mock is

   package SU renames Ada.Strings.Unbounded;

   function Create return People_Carrier.Utils.Mock.Reference
   is (new People_Carrier.Utils.Mock.Object);

   function Is_A_People_Carrier (This         : in     Mock.Object;
                                 Traveller_Id : in     Agent.Agent_Id)
   return Boolean is
   begin
      if not This.Return_Values.Is_A_People_Carrier.Contains (Traveller_Id)
      then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception (
            Function_Param => "Traveller_Id: " & SU.To_String (Traveller_Id),
            Function_Name  => "Is_A_People_Carrier",
            Package_Name   => "Active.People_Carrier.Utils.Mock");
      end if;

      return This.Return_Values.Is_A_People_Carrier.Element (Traveller_Id);
   end Is_A_People_Carrier;

   function Get_Passengers (This       : in     Mock.Object;
                            Carrier_Id : in     Agent.Agent_Id)
   return Agent_Id_List.List is
   begin
      if not This.Return_Values.Passengers_Existence then
         Raise_Missing_Return_Value_For_Mocked_Procedure_Exception (
            Parameter_Name => "Passengers",
            Procedure_Name => "Get_Passengers",
            Package_Name   => "Active.People_Carrier.Utils.Mock");
      end if;

      return This.Return_Values.Passengers;
   end Get_Passengers;

   procedure Board (This       : in     Mock.Object;
                    Carrier_Id : in     Agent.Agent_Id;
                    Incomer_Id : in     Agent.Agent_Id;
                    Boarded    :    out Boolean) is
   begin
      if not This.Return_Values.Boarded_Existence or
         not This.Return_Values.Boarded.Contains (Incomer_Id)
      then
         Raise_Missing_Return_Value_For_Mocked_Procedure_Exception (
            Parameter_Name => "Boarded",
            Procedure_Name => "Board",
            Package_Name   => "Active.People_Carrier.Utils.Mock");
      end if;

      Boarded := This.Return_Values.Boarded.Element (Incomer_Id);
   end Board;

   procedure Free (This         : in out Mock.Object;
                   Carrier_Id   : in     Agent.Agent_Id;
                   Passenger_Id : in     Agent.Agent_Id;
                   Freed        :    out Boolean) is
   begin
      if not This.Return_Values.Freed_Existence  or
         not This.Return_Values.Freed.Contains (Passenger_Id)
      then
         Raise_Missing_Return_Value_For_Mocked_Procedure_Exception (
            Parameter_Name => "Freed",
            Procedure_Name => "Free",
            Package_Name   => "Active.People_Carrier.Utils.Mock");
      end if;

      Freed := This.Return_Values.Freed.Element (Passenger_Id);
      This.Return_Values.Free_Called := True;
   end Free;

   function Is_Carrier_Full (This       : in out Mock.Object;
                             Carrier_Id : in     Agent.Agent_Id)
      return Boolean is
   begin
      if not This.Return_Values.Is_Carrier_Full.Contains (Carrier_Id)
      then
         Raise_Missing_Return_Value_For_Mocked_Procedure_Exception (
            Parameter_Name => "Returned value",
            Procedure_Name => "Is_Carrier_Full",
            Package_Name   => "Active.People_Carrier.Utils.Mock");
      end if;

      return This.Return_Values.Is_Carrier_Full.Element (Carrier_Id);
   end Is_Carrier_Full;

   procedure Set_Value_For_Is_A_People_Carrier (
      This                : in out Mock.Object;
      Traveller_Id        : in     Agent.Agent_Id;
      Is_A_People_Carrier : in     Boolean) is
   begin
      This.Return_Values.Is_A_People_Carrier.Include (
         Traveller_Id, Is_A_People_Carrier);
      This.Return_Values.Is_A_People_Carrier_Existence := True;
   end Set_Value_For_Is_A_People_Carrier;

   procedure Set_List_For_Get_Passengers (
      This       : in out Mock.Object;
      Passengers : in     Agent_Id_List.List) is
   begin
      This.Return_Values.Passengers := Passengers;
      This.Return_Values.Passengers_Existence := True;
   end;

   procedure Set_Value_For_Board (
      This       : in out Mock.Object;
      Boarder_Id : in     Agent.Agent_Id;
      Boarded    : in     Boolean) is
   begin
      This.Return_Values.Boarded.Insert (Boarder_Id, Boarded);
      This.Return_Values.Boarded_Existence := True;
   end;

   procedure Set_Value_For_Free (
      This         : in out Mock.Object;
      Passenger_Id : in     Agent.Agent_Id;
      Freed        : in     Boolean) is
   begin
      This.Return_Values.Freed.Insert (Passenger_Id, Freed);
      This.Return_Values.Freed_Existence := True;
   end;

   procedure Set_Value_For_Is_Carrier_Full (
      This       : in out Mock.Object;
      Carrier_Id : in     Agent.Agent_Id;
      Full       : in     Boolean) is
  begin
      This.Return_Values.Is_Carrier_Full.Include (Carrier_Id, Full);
      This.Return_Values.Is_Carrier_Full_Existence := True;
  end;

   function Get_Free_Called (This  : in out Mock.Object) return Boolean
   is (This.Return_Values.Free_Called);

end Active.People_Carrier.Utils.Mock;
