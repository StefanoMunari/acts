with Active.Traveller;

with Reactive.District;

package body Active.Bus_Service.Utils is

   function Get_Instance (
      District : access Reactive.District.Object'Class := null)
   return Bus_Service.Utils.Reference is
   begin
      if Instance = null then
         Instance := new Bus_Service.Utils.Object;
      end if;

      if District = null then
         Instance.District := Reactive.District.Get_Instance;
      else
         Instance.District := District;
      end if;

      return Instance;
   end Get_Instance;

   function Is_A_Bus_Service (
      This         : in out Bus_Service.Utils.Object;
      Traveller_Id : in     Agent.Agent_Id)
   return Boolean is
   begin
   declare
      Test_Variable : access Bus_Service.Object'Class :=
         Bus_Service.Reference (
            This.District.Find_Traveller_By_Id (Traveller_Id));
      begin
         return True;
      end;
   exception
      when Constraint_Error =>
         return False;
   end Is_A_Bus_Service;

   procedure On_Bus_Stop (This         : in out Bus_Service.Utils.Object;
                          Traveller_Id : in     Agent.Agent_Id) is
      User : access Bus_Service.Object'Class :=
         Bus_Service.Reference (
            This.District.Find_Traveller_By_Id (Traveller_Id));
   begin
      User.On_Bus_Stop;
   end On_Bus_Stop;

end Active.Bus_Service.Utils;
