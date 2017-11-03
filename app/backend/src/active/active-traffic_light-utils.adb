with Reactive.District;

package body Active.Traffic_Light.Utils is

   function Get_Instance (
      District : access Reactive.District.Object'Class := null)
   return Traffic_Light.Utils.Reference is
   begin
      if Instance = null then
         Instance := new Traffic_Light.Utils.Object;
      end if;

      if District = null then
         Instance.District := Reactive.District.Get_Instance;
      else
         Instance.District := District;
      end if;

      return Instance;
   end Get_Instance;

   function Is_A_Traffic_Light (
      This      : in out Traffic_Light.Utils.Object;
      Active_Id : in     Agent.Agent_Id)
   return Boolean is
   begin
      return This.District.Contains_Traffic_Light (Active_Id);
   end Is_A_Traffic_Light;

end Active.Traffic_Light.Utils;
