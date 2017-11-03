with Active.Traveller;
with Active.Traveller.Vehicle.Bus;

with Reactive.District;

package body Active.Traveller.Vehicle.Bus.Utils is

   function Get_Instance (
      District_Ref : access Reactive.District.Object'Class := null)
   return Bus.Utils.Reference is
   begin
      if Instance = null then
         Instance := new Bus.Utils.Object;
      end if;

      if District_Ref = null then
         Instance.District_Ref := Reactive.District.Get_Instance;
      else
         Instance.District_Ref := District_Ref;
      end if;

      return Instance;
   end Get_Instance;

   function Is_A_Bus (
      This         : in out Bus.Utils.Object;
      Traveller_Id : in     Agent.Agent_Id)
   return Boolean is
   begin
      declare
         Test_Variable : access Bus.Object'Class :=
            Bus.Reference (
               This.District_Ref.Find_Traveller_By_Id (Traveller_Id));
      begin
         return True;
      end;
   exception
      when Constraint_Error =>
         return False;
   end Is_A_Bus;

   function Get_Route_Stops (
      This   : in Bus.Utils.Object;
      Bus_Id : in Agent.Agent_Id)
   return Infra_Id_List.List
   is
      Bus_Ref : access Bus.Object'Class :=
         Bus.Reference (
            This.District_Ref.Find_Traveller_By_Id (Bus_Id));
   begin
     return Bus_Ref.Get_Route_Stops;
   end Get_Route_Stops;

end Active.Traveller.Vehicle.Bus.Utils;
