with Active.Traveller;

with Reactive.District;

package body Active.People_Carrier.Utils is

   function Get_Instance (
      District : access Reactive.District.Object'Class := null)
   return People_Carrier.Utils.Reference is
   begin
      if Instance = null then
         Instance := new People_Carrier.Utils.Object;
      end if;

      if District = null then
         Instance.District := Reactive.District.Get_Instance;
      else
         Instance.District := District;
      end if;

      return Instance;
   end Get_Instance;

   function Is_A_People_Carrier (
      This         : in     People_Carrier.Utils.Object;
      Traveller_Id : in     Agent.Agent_Id)
   return Boolean
   is
   begin
      declare
         Test_Variable : access People_Carrier.Object'Class :=
            People_Carrier.Reference
               (This.District.Find_Traveller_By_Id (Traveller_Id));
      begin
         return True;
      end;
   exception
      when Constraint_Error =>
         return False;
   end Is_A_People_Carrier;

   function Get_Passengers (
      This       : in     People_Carrier.Utils.Object;
      Carrier_Id : in     Agent.Agent_Id)
   return Agent_Id_List.List
   is
      People_Carrier_Ref : People_Carrier.Reference :=
         People_Carrier.Reference
            (This.District.Find_Traveller_By_Id (Carrier_Id));
   begin
      return People_Carrier_Ref.Get_Passengers;
   end Get_Passengers;

   procedure Board (
      This       : in     People_Carrier.Utils.Object;
      Carrier_Id : in     Agent.Agent_Id;
      Incomer_Id : in     Agent.Agent_Id;
      Boarded    :    out Boolean)
   is
      People_Carrier_Ref : People_Carrier.Reference :=
         People_Carrier.Reference (
            This.District.Find_Traveller_By_Id (Carrier_Id));
   begin
      People_Carrier_Ref.Board (Incomer_Id, Boarded);
   end Board;

   procedure Free (
      This         : in out People_Carrier.Utils.Object;
      Carrier_Id   : in     Agent.Agent_Id;
      Passenger_Id : in     Agent.Agent_Id;
      Freed        :    out Boolean)
   is
      People_Carrier_Ref : People_Carrier.Reference :=
         People_Carrier.Reference (
            This.District.Find_Traveller_By_Id (Carrier_Id));
   begin
      People_Carrier_Ref.Free (Passenger_Id, Freed);
   end Free;

   function Get_Max_Number_Of_Passengers (
      This       : in People_Carrier.Utils.Object;
      Carrier_Id : in Agent.Agent_Id)
   return Natural is
      People_Carrier_Ref : People_Carrier.Reference :=
         People_Carrier.Reference (
            This.District.Find_Traveller_By_Id (Carrier_Id));
   begin
     return People_Carrier_Ref.Get_Max_Number_Of_Passengers;
   end Get_Max_Number_Of_Passengers;

   function Is_Carrier_Full (This       : in out People_Carrier.Utils.Object;
                             Carrier_Id : in     Agent.Agent_Id)
   return Boolean
   is
      Passengers     : Agent_Id_List.List := This.Get_Passengers (Carrier_Id);
      Max_Passengers : Natural
         := This.Get_Max_Number_Of_Passengers (Carrier_Id);
   begin
      return Natural (Passengers.Length) = Max_Passengers;
   end Is_Carrier_Full;

end Active.People_Carrier.Utils;
