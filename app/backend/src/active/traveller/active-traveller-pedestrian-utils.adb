with Active.Traveller;

with Reactive.District;

package body Active.Traveller.Pedestrian.Utils is

   function Get_Instance (
      District : access Reactive.District.Object'Class := null)
   return Pedestrian.Utils.Reference is
   begin
      if Instance = null then
         Instance := new Pedestrian.Utils.Object;
      end if;

      if District = null then
         Instance.District := Reactive.District.Get_Instance;
      else
         Instance.District := District;
      end if;

      return Instance;
   end;

   function Is_A_Pedestrian (This         : in Pedestrian.Utils.Object;
                             Traveller_Id : in Agent.Agent_Id) return Boolean is
   begin
   declare
      Test_Variable : access Pedestrian.Object'Class :=
         Pedestrian.Reference
            (This.District.Find_Traveller_By_Id (Traveller_Id));
      begin
         return True;
      end;
   exception
      when CONSTRAINT_ERROR =>
         return False;
   end Is_A_Pedestrian;

   procedure Stop_Waiting (
      This         : in out Pedestrian.Utils.Object;
      Traveller_Id : in     Agent.Agent_Id)
   is
      Pedestrian_Ref : Pedestrian.Reference;
   begin
      Pedestrian_Ref :=
         Pedestrian.Reference (
            This.District.Find_Traveller_By_Id (Traveller_Id));
      Pedestrian_Ref.Stop_Waiting;
   end Stop_Waiting;

   not overriding
   procedure Recompute_Travel (
      This          : Pedestrian.Utils.Object;
      Traveller_Id  : Agent.Agent_Id)
   is
      Pedestrian_Ref : Pedestrian.Reference;
   begin
      Pedestrian_Ref :=
         Pedestrian.Reference (
            This.District.Find_Traveller_By_Id (Traveller_Id));
      Pedestrian_Ref.Recompute_Travel;
   end Recompute_Travel;

end Active.Traveller.Pedestrian.Utils;
