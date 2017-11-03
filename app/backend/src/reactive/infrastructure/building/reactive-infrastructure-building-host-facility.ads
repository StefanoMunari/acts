with Active.Agent;
with Active.People_Carrier.Utils;
with Active.Traveller.Utils;

with Interface_Layer.Remote.Query_Builder;
with Interface_Layer.Remote.Stub;
with Interface_Layer.Wrappers.Application.Abstract_Factory;

with Reactive.District;
with Reactive.Infrastructure.Building.Host.Utils;
with Reactive.Infrastructure.Building.Host.Use_Carrier_Strategy;
with Reactive.Infrastructure.Building.Parking_Manager;

with Shared.Agent_Id_Set;
with Shared.Slice;

package Reactive.Infrastructure.Building.Host.Facility is

   package Agent
      renames Active.Agent;
   package People_Carrier_Utils
      renames Active.People_Carrier.Utils;
   package Traveller_Utils_Pkg
      renames Active.Traveller.Utils;
   package Remote_Query_Builder
      renames Interface_Layer.Remote.Query_Builder;
   package Remote_Stub
      renames Interface_Layer.Remote.Stub;
   package App_Wrapper
      renames Interface_Layer.Wrappers.Application;
   package District_Pkg
      renames Reactive.District;
   package Parking_Manager
      renames Reactive.Infrastructure.Building.Parking_Manager;
   package Agent_Id_Set
      renames Shared.Agent_Id_Set;
   package UC_Strategy
      renames
         Reactive.Infrastructure.Building.Host.Use_Carrier_Strategy;
   package Slice
      renames Shared.Slice;

   use Active.Space_Master.Next_Action_Type;

   type Object is
      new Host.Object
   with private;
   type Reference is access all Facility.Object'Class;

   not overriding
   function Create (
      Id                   : Infra_Id;
      Parking_Ref          : Parking_Manager.Reference;
      Host_Utils_Ref       : access Host.Utils.Object'Class := null;
      PC_Utils             : access People_Carrier_Utils.Object'Class := null;
      District             : access District_Pkg.Object'Class := null;
      Traveller_Utils      : access Traveller_Utils_Pkg.Object'Class := null;
      Use_Carrier_Strategy : access UC_Strategy.Object'Class := null;
      Stub            : access Remote_Stub.Object'Class := null;
      Query_Builder   : access Remote_Query_Builder.Object'Class := null;
      Wrapper_Factory :
         access App_Wrapper.Abstract_Factory.Object'Class := null)
   return Facility.Reference;

   overriding
   function Get_Id (This : in Facility.Object)
   return Infra_Id;

   overriding
   function Get_Parking (This : in out Facility.Object)
   return Parking_Manager.Reference;

   overriding
   procedure Stop_Over (This       : in out Facility.Object;
                        Travellers : in     Agent_Id_List.List);


   overriding
   function Exit_Building (This         : in out Facility.Object;
                           Traveller_Id : in     Agent.Agent_Id;
                           Exiting_Id   :    out Agent.Agent_Id)
   return Next_Action;

   overriding
   procedure Put_Stopping_Traveller (This         : in out Facility.Object;
                                     Traveller_Id : in     Agent.Agent_Id);

   overriding
   procedure Accessible_By (
      This       : in out Facility.Object;
      Stretch_Id : in     Infra_Id;
      Stretch_T  : in     Stretch_Type);

   overriding
   function Dump (This : Facility.Object) return G_JSON.JSON_Value;

private

   procedure Try_To_Book_Parking (
      This         : in out Facility.Object;
      Vehicle_Id   : in     Agent.Agent_Id;
      Traveller_Id : in     Agent.Agent_Id);

   protected type Hosted_Travellers is

      procedure Add_Traveller    (Traveller_Id : in Agent.Agent_Id);

      procedure Remove_Traveller (Traveller_Id : in     Agent.Agent_Id;
                                  Found        :    out Boolean);

      function Contains_Traveller (Traveller_Id : in Agent.Agent_Id)
      return Boolean;

      function Get_Travellers return G_JSON.JSON_Array;

   private
      Travellers : Agent_Id_Set.Set;
   end Hosted_Travellers;

   type Object is
      new Host.Object
   with record
      Id                   : Infra_Id;
      Stretches            : Slice.Map;
      Hosted_Travellers    : access Facility.Hosted_Travellers;
      Parking              : Parking_Manager.Reference;
      Host_Utils_Ref       : access Host.Utils.Object'Class := null;
      PC_Utils             : access People_Carrier_Utils.Object'Class := null;
      District             : access District_Pkg.Object'Class := null;
      Traveller_Utils      : access Traveller_Utils_Pkg.Object'Class := null;
      Use_Carrier_Strategy : access UC_Strategy.Object'Class := null;
      Query_Builder    : access Remote_Query_Builder.Object'Class := null;
      Stub             : access Remote_Stub.Object'Class := null;
      Wrapper_Factory  :
         access App_Wrapper.Abstract_Factory.Object'Class := null;
   end record;

end Reactive.Infrastructure.Building.Host.Facility;
