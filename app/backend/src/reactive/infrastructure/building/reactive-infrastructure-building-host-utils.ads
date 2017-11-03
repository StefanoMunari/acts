with Active.Agent;
with Active.People_Carrier.Utils;

with Interface_Layer.Remote.Stub;
with Interface_Layer.Wrappers.Application.Abstract_Factory;

limited with Reactive.District;

package Reactive.Infrastructure.Building.Host.Utils is

   package Agent           renames Active.Agent;
   package People_Carrier  renames Active.People_Carrier;
   package Stub_Pkg        renames Interface_Layer.Remote.Stub;
   package App_Wrapper_Pkg renames Interface_Layer.Wrappers.Application;
   package Wrapper_Fac_Pkg renames App_Wrapper_Pkg.Abstract_Factory;
   use Active.Space_Master.Next_Action_Type;

   type Object (<>) is tagged limited private;
   type Reference is access all Host.Utils.Object'Class;

   function Get_Instance (
      PC_Utils        : access People_Carrier.Utils.Object'Class := null;
      District_Ref    : access Reactive.District.Object'Class := null;
      Wrapper_Factory : access Wrapper_Fac_Pkg.Object'Class := null;
      Stub            : access Stub_Pkg.Object'Class := null)
   return Host.Utils.Reference;

   not overriding
   function Get_Parking (This    : in out Host.Utils.Object;
                         Host_Id : in     Infra_Id)
   return Parking_Manager.Reference;

   not overriding
   function Stop_Over (This         : in out Host.Utils.Object;
                       Host_Id      : in     Infra_Id;
                       Traveller_Id : in     Agent.Agent_Id)
   return Agent_Id_List.List;

   not overriding
   function Exit_Building (This         : in out Host.Utils.Object;
                           Host_Id      : in     Infra_Id;
                           Traveller_Id : in     Agent.Agent_Id;
                           Vehicle_Id   :    out Agent.Agent_Id)
   return Next_Action;

   not overriding
   procedure Accessible_By (
      This       : in out Host.Utils.Object;
      Host_Id    : in     Infra_Id;
      Stretch_Id : in     Infra_Id;
      Stretch_T  : in     Stretch_Type);

   not overriding
   function Dump (
      This    : Host.Utils.Object;
      Host_Id : Infra_Id)
   return G_JSON.JSON_Value;

private
   type Object is tagged limited record
      PC_Utils        : access Active.People_Carrier.Utils.Object'Class;
      District_Ref    : access Reactive.District.Object'Class;
      Wrapper_Factory : access Wrapper_Fac_Pkg.Object'Class := null;
      Stub            : access Stub_Pkg.Object'Class;
   end record;

   Instance : Host.Utils.Reference;

end Reactive.Infrastructure.Building.Host.Utils;
