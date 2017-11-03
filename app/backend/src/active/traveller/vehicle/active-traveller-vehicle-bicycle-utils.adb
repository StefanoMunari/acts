with Active.Traveller;

with Reactive.District;

package body Active.Traveller.Vehicle.Bicycle.Utils is

   function Get_Instance (
      District : access Reactive.District.Object'Class := null)
   return Bicycle.Utils.Reference is
   begin
      if Instance = null then
         Instance := new Bicycle.Utils.Object;
      end if;

      if District = null then
         Instance.District := Reactive.District.Get_Instance;
      else
         Instance.District := District;
      end if;

      return Instance;
   end;

   function Is_A_Bicycle (This         : in out Bicycle.Utils.Object;
                          Traveller_Id : in     Agent.Agent_Id) return Boolean is
   begin
      declare
         Test_Variable : access Bicycle.Object'Class :=
            Bicycle.Reference
               (This.District.Find_Traveller_By_Id (Traveller_Id));
      begin
         return True;
      end;
   exception
      when Constraint_Error =>
         return False;
   end;

end Active.Traveller.Vehicle.Bicycle.Utils;
