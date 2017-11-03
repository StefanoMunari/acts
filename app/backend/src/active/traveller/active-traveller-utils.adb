with Active.Space_Master;

with Reactive.District;

package body Active.Traveller.Utils is

   function Get_Instance (
      District : access Reactive.District.Object'Class := null)
   return Traveller.Utils.Reference is
      District_Ref : access Reactive.District.Object'Class := District;
   begin
      if Instance = null then
         Instance := new Traveller.Utils.Object;
      end if;

      if District_Ref = null then
         District_Ref := Reactive.District.Get_Instance;
      else
         Instance.District_Ref := District_Ref;
      end if;

      return Instance;
   end Get_Instance;

   function Get_Stretch_Type (
      This         : in Traveller.Utils.Object;
      Traveller_Id : in Agent.Agent_Id)
   return Stretch_Type is
   begin
     return This
               .District_Ref
               .Find_Traveller_By_Id (Traveller_Id)
               .Get_Stretch_Type;
   end Get_Stretch_Type;

   procedure Is_Affected_By_Semaphores (
      This         : in     Traveller.Utils.Object;
      Traveller_Id : in     Agent.Agent_Id;
      Affected     :    out Boolean) is
   begin
      Affected := This.District_Ref
                  .Find_Traveller_By_Id (Traveller_Id)
                  .Is_Affected_By_Traffic_Lights;
   end Is_Affected_By_Semaphores;

   procedure Consume_Step (
      This         : in out Traveller.Utils.Object;
      Traveller_Id : in     Agent.Agent_Id) is
   begin
      This.District_Ref
         .Find_Traveller_By_Id (Traveller_Id)
         .Consume_Step;
   end Consume_Step;

   function Get_Next_Step (This         : in     Traveller.Utils.Object;
                           Traveller_Id : in     Agent.Agent_Id)
   return Infra_Id
   is
      Traveller_Ref : Traveller.Reference;
   begin
      Traveller_Ref := This.District_Ref
                           .Find_Traveller_By_Id (Traveller_Id);
      return Traveller_Ref.Travel_Ref.Get_Current_Step_Id;
   end Get_Next_Step;

   function Get_Position (This         : in     Traveller.Utils.Object;
                          Traveller_Id : in     Agent.Agent_Id)
   return Infra_Id is
   begin
      return This.District_Ref
            .Find_Traveller_By_Id (Traveller_Id)
            .Get_Position;
   end Get_Position;

   procedure Set_Position (This         : in     Traveller.Utils.Object;
                           Traveller_Id : in     Agent.Agent_Id;
                           New_Position : in     Infra_Id) is
   begin
      This.District_Ref
         .Find_Traveller_By_Id (Traveller_Id)
         .Set_Position (New_Position);
   end Set_Position;

   function Get_Maximum_Speed (
      This         : in     Traveller.Utils.Object;
      Traveller_Id : in     Agent.Agent_Id) return Natural
   is
   begin
      return This.District_Ref
            .Find_Traveller_By_Id (Traveller_Id)
            .Get_Maximum_Speed;
   end Get_Maximum_Speed;

   function Get_Current_Speed (
      This         : in     Traveller.Utils.Object;
      Traveller_Id : in     Agent.Agent_Id) return Natural
   is
   begin
      return This.District_Ref
            .Find_Traveller_By_Id (Traveller_Id)
            .Get_Current_Speed;
   end Get_Current_Speed;

   procedure Set_Current_Speed (This         : in     Traveller.Utils.Object;
                                Traveller_Id : in     Agent.Agent_Id;
                                New_Speed    : in     Natural) is
   begin
      This.District_Ref
         .Find_Traveller_By_Id (Traveller_Id)
         .Set_Current_Speed (New_Speed);
   end Set_Current_Speed;

   procedure Set_Travel (This         : in out Traveller.Utils.Object;
                         Traveller_Id : in     Agent.Agent_Id;
                         Travel       : access Active.Travel.Object'Class) is
   begin
      This.District_Ref
         .Find_Traveller_By_Id (Traveller_Id)
         .Set_Travel (Travel);
   end Set_Travel;

   function Get_Travel_Source (
      This         : in     Traveller.Utils.Object;
      Traveller_Id : in     Agent.Agent_Id) return Slice.Map
   is
   begin
      return This.District_Ref
            .Find_Traveller_By_Id (Traveller_Id)
            .Get_Travel_Source;
   end Get_Travel_Source;

   function Get_Travel_Destination (
      This         : in     Traveller.Utils.Object;
      Traveller_Id : in     Agent.Agent_Id) return Slice.Map
   is
   begin
      return This.District_Ref
            .Find_Traveller_By_Id (Traveller_Id)
            .Get_Travel_Destination;
   end Get_Travel_Destination;

   function Look_Ahead_Step (
      This         : in Traveller.Utils.Object;
      Traveller_Id : in Agent.Agent_Id;
      Index        : in Natural)
   return Infra_Id is
   begin
      return This.District_Ref
                 .Find_Traveller_By_Id (Traveller_Id)
                 .Look_Ahead_Step (Index);
   end Look_Ahead_Step;

   function Does_Travel_Contain_Step (This         : in Traveller.Utils.Object;
                                      Traveller_Id : in     Agent.Agent_Id;
                                      Step         : in Infra_Id)
   return Boolean
   is
   begin
      return This.District_Ref
                 .Find_Traveller_By_Id (Traveller_Id)
                 .Does_Travel_Contain_Step (Step);
   end Does_Travel_Contain_Step;

   function Does_Travel_Contain_Steps (
      This         : in     Traveller.Utils.Object;
      Traveller_Id : in     Agent.Agent_Id;
      Step         : in     Slice.Map)
   return Boolean is
   begin
     return This.District_Ref
            .Find_Traveller_By_Id (Traveller_Id)
            .Does_Travel_Contain_Steps (Step);
   end Does_Travel_Contain_Steps;

   procedure Modify_Travel_Beginning (
      This          : Traveller.Utils.Object;
      Traveller_Id  : Agent.Agent_Id;
      New_Beginning : Infra_Id_List.List) is
   begin
      This.District_Ref
          .Find_Traveller_By_Id (Traveller_Id)
          .Modify_Travel_Beginning (New_Beginning);
   end Modify_Travel_Beginning;

   not overriding
   procedure Erase_Route (
      This         : in out Traveller.Utils.Object;
      Traveller_Id :        Agent.Agent_Id) is
   begin
      This.District_Ref
          .Find_Traveller_By_Id (Traveller_Id)
          .Erase_Route;
   end Erase_Route;

   function Get_List_From_Slice (This         : in     Traveller.Utils.Object;
                                 Traveller_Id : in     Agent.Agent_Id;
                                 Slice_Obj    : in     Slice.Map)
      return Infra_Id_List.List
    is
    begin
      return This.District_Ref
            .Find_Traveller_By_Id (Traveller_Id)
            .Get_List_From_Slice (Slice_Obj);
    end Get_List_From_Slice;

   procedure Defer (This         : in out Traveller.Utils.Object;
                    Traveller_Id : in     Agent.Agent_Id;
                    Retry_Action : in     Boolean)
   is
      Master : Active.Space_Master.Reference;
   begin
      Master := Active.Space_Master.Get_Instance (This.District_Ref);
      Master.Defer (Traveller_Id, Retry_Action);
   end Defer;

   not overriding
   function Get_Size (
      This         : in Traveller.Utils.Object;
      Traveller_Id : in Agent.Agent_Id) return Natural is
   begin
      return This.District_Ref
             .Find_Traveller_By_Id (Traveller_Id)
             .Get_Size;
   end Get_Size;

end Active.Traveller.Utils;
