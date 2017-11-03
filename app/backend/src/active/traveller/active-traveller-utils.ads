------------------------------------------------------------------------------
-- @author  <gianmarco.midena@gmail.com>
-- @author  <stefanomunari.sm@gmail.com>
-- @author  <valle.sebastiano93@gmail.com>
-- @context application-backend::active-traveller-utils
-- @purpose Helper package which provides methods to access travellers
-- @interface Get_Instance (Object, Agent_id):
--              returns the singleton instance
-- @dependencies application-backend::active-agent
--               application-backend::reactive-district
--               application-backend::shared-infra_id_list
--               application-backend::shared-slice
-- @details Singleton. Most of the API is omitted since self-explanatory, it
--          just consists of forwarded invocations to the actual travellers
------------------------------------------------------------------------------

with Active.Agent;

limited with Reactive.District;

with Shared.Infra_Id_List;
with Shared.Slice;

package Active.Traveller.Utils is

   package Agent renames Active.Agent;
   package Infra_Id_List renames Shared.Infra_Id_List;
   package Slice renames Shared.Slice;

   type Object (<>) is tagged limited private;
   type Reference is access all Traveller.Utils.Object'Class;

   function Get_Instance (
      District : access Reactive.District.Object'Class := null)
   return Traveller.Utils.Reference;

   not overriding
   function Get_Stretch_Type (
      This         : in Traveller.Utils.Object;
      Traveller_Id : in Agent.Agent_Id)
   return Stretch_Type;

   not overriding
   procedure Is_Affected_By_Semaphores (
      This         : in     Traveller.Utils.Object;
      Traveller_Id : in     Agent.Agent_Id;
      Affected     :    out Boolean);

   not overriding
   procedure Consume_Step (
      This         : in out Traveller.Utils.Object;
      Traveller_Id : in     Agent.Agent_Id);

   not overriding
   function Get_Next_Step (This         : in     Traveller.Utils.Object;
                           Traveller_Id : in     Agent.Agent_Id)
   return Infra_Id;

   not overriding
   function Get_Position (This         : in     Traveller.Utils.Object;
                          Traveller_Id : in     Agent.Agent_Id)
   return Infra_Id;

   not overriding
   procedure Set_Position (This         : in     Traveller.Utils.Object;
                           Traveller_Id : in     Agent.Agent_Id;
                           New_Position : in     Infra_Id);

   not overriding
   function Get_Maximum_Speed (This         : in     Traveller.Utils.Object;
                               Traveller_Id : in     Agent.Agent_Id)
   return Natural;

   not overriding
   function Get_Current_Speed (This         : in     Traveller.Utils.Object;
                               Traveller_Id : in     Agent.Agent_Id)
   return Natural;

   not overriding
   procedure Set_Current_Speed (This         : in     Traveller.Utils.Object;
                                Traveller_Id : in     Agent.Agent_Id;
                                New_Speed    : in     Natural);

   not overriding
   procedure Set_Travel (This         : in out Traveller.Utils.Object;
                         Traveller_Id : in     Agent.Agent_Id;
                         Travel       : access Active.Travel.Object'Class);

   not overriding
   function Get_Travel_Source (This         : in     Traveller.Utils.Object;
                               Traveller_Id : in     Agent.Agent_Id)
   return Slice.Map;

   not overriding
   function Get_Travel_Destination (
      This         : in     Traveller.Utils.Object;
      Traveller_Id : in     Agent.Agent_Id)
   return Slice.Map;

   not overriding
   function Look_Ahead_Step (
      This         : in Traveller.Utils.Object;
      Traveller_Id : in Agent.Agent_Id;
      Index        : in Natural)
   return Infra_Id;

   not overriding
   function Does_Travel_Contain_Step (This         : in Traveller.Utils.Object;
                                      Traveller_Id : in     Agent.Agent_Id;
                                      Step         : in Infra_Id)
   return Boolean;

   not overriding
   function Does_Travel_Contain_Steps (
      This         : in     Traveller.Utils.Object;
      Traveller_Id : in     Agent.Agent_Id;
      Step         : in     Slice.Map)
   return Boolean;

   not overriding
   procedure Modify_Travel_Beginning (
      This          : Traveller.Utils.Object;
      Traveller_Id  : Agent.Agent_Id;
      New_Beginning : Infra_Id_List.List);

   not overriding
   procedure Erase_Route (
      This         : in out Traveller.Utils.Object;
      Traveller_Id :        Agent.Agent_Id);

   not overriding
   function Get_List_From_Slice (This         : in     Traveller.Utils.Object;
                                 Traveller_Id : in     Agent.Agent_Id;
                                 Slice_Obj    : in     Slice.Map)
   return Infra_Id_List.List;

   not overriding
   procedure Defer (This         : in out Traveller.Utils.Object;
                    Traveller_Id : in     Agent.Agent_Id;
                    Retry_Action : in     Boolean);

   not overriding
   function Get_Size (
      This         : in Traveller.Utils.Object;
      Traveller_Id : in Agent.Agent_Id) return Natural;

private
   type Object is tagged limited record
      District_Ref : access Reactive.District.Object'Class;
   end record;

   Instance : Traveller.Utils.Reference := null;

end Active.Traveller.Utils;
