with Active.Traveller;

with Reactive.District;

package body Active.Space_Master is

   function Get_Instance (
      District_Ref : access Reactive.District.Object'Class := null)
   return Active.Space_Master.Reference
   is
   begin
      if Instance = null then
         Instance := new Active.Space_Master.Object;
      end if;

      Instance.District_Ref := District_Ref;
      if Instance.District_Ref = null then
         Instance.District_Ref := Reactive.District.Get_Instance;
      end if;

      return Active.Space_Master.Reference (Instance);
   end Get_Instance;

   function Get_Max_Speed (This : in Space_Master.Object) return Float is
   begin -- Get_Max_Speed
      return This.Max_Speed_Among_All_Entities;
   end Get_Max_Speed;

   procedure Set_Max_Speed (This               : in out Space_Master.Object;
                            Possible_Max_Speed : in     Natural) is
   begin -- Set_Max_Speed
      if Float (Possible_Max_Speed) > This.Max_Speed_Among_All_Entities then
         This.Max_Speed_Among_All_Entities := Float (Possible_Max_Speed);
      end if;
   end Set_Max_Speed;

   -- resets Max_Speed_Among_All_Entities
   procedure Clear (This : in out Space_Master.Object) is
   begin -- Clear
      This.Max_Speed_Among_All_Entities := 0.0;
   end Clear;

   procedure Defer (This         : in Space_Master.Object;
                   Traveller_Id  : in Agent.Agent_Id;
                   Retry_Action  : in Boolean)
   is
      The_Traveller : aliased Active.Traveller.Reference;
      Deferred_To   : Float := 1.0;
   begin
      The_Traveller := This.District_Ref.Find_Traveller_By_Id (Traveller_Id);
      if not Retry_Action then
         Deferred_To := This.Get_Deferral (Float (The_Traveller.Get_Current_Speed));
      end if;

   -- Set new deferred moment for next action
      The_Traveller.Set_Scheduled_For (Deferred_To);
      -- Schedule next traveller move
      This.District_Ref.Schedule (The_Traveller);
   end Defer;

   procedure Defer (This         : in Space_Master.Object;
                    Traveller_Id : in Agent.Agent_Id;
                    Deferred_To  : in Float) is
      The_Traveller : aliased Active.Traveller.Reference
         := This.District_Ref.Find_Traveller_By_Id (Traveller_Id);
   begin
   -- Set new deferred moment for next action
      The_Traveller.Set_Scheduled_For (Deferred_To);
      -- Schedule next traveller move
      This.District_Ref.Schedule (The_Traveller);
   end Defer;

   function Get_Deferral (This  : in Space_Master.Object;
                          Speed : in Float) return Float is
   begin -- Get_Deferral
      return (This.Max_Speed_Among_All_Entities / Speed);
   end Get_Deferral;

end Active.Space_Master;
