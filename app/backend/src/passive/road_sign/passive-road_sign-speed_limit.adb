package body Passive.Road_Sign.Speed_Limit is

   not overriding
   function Create (
      Limit           : in Natural;
      Traveller_Utils : access Traveller.Utils.Object'Class := null)
   return Passive.Road_Sign.Speed_Limit.Reference is
      New_Sign : Speed_Limit.Reference := new Speed_Limit.Object;
   begin
      New_Sign.Limit           := Limit;

      New_Sign.Traveller_Utils := Traveller.Utils.Reference (Traveller_Utils);
      if Traveller_Utils = null then
         New_Sign.Traveller_Utils := Active.Traveller.Utils.Get_Instance;
      end if;

      return New_Sign;
   end;

   procedure Apply (This       : in out Speed_Limit.Object;
                    Traveller  : in     Agent.Agent_Id) is
   begin
      If This.Traveller_Utils.Get_Current_Speed (Traveller) > This.Limit then
         This.Traveller_Utils.Set_Current_Speed (
            Traveller_Id => Traveller,
            New_Speed    => This.Limit);
      end if;
   end Apply;

   function Get_Limit (This : in Speed_Limit.Object)
    return Natural is (This.Limit);

   function Dump (This : Speed_Limit.Object) return G_JSON.JSON_Value
   is
      JSON : G_JSON.JSON_Value := G_JSON.Create_Object;
   begin
      JSON.Set_Field (Speed_Limit_Field, This.Limit);

      return JSON;
   end Dump;

end Passive.Road_Sign.Speed_Limit;
