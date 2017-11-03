with Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;

with Interface_Layer.Wrappers.Application;
with Interface_Layer.Utils.Types;
with Interface_Layer.Service.Pipelines.Handler.Concrete_Factory;
with Interface_Layer.Wrappers.Application.Concrete_Factory;

with Reactive.Infrastructure_Registry;
with Reactive.Infrastructure_Registry.Exceptions;
use Reactive.Infrastructure_Registry.Exceptions;
with Reactive.Traveller_Registry;
with Reactive.Infrastructure.Lane.Decoration.Lane_Sign_Decorator;

with Scheduling.Remote.Callback.Failure;
with Scheduling.Remote.Callback.Failure.Tread;
with Scheduling.Remote.Callback.Success;
with Scheduling.Remote.Callback.Success.Tread;
with Scheduling.Scheduler;

with Shared.Callback_Pair;

package body Reactive.District is

   package Types                   renames Interface_Layer.Utils.Types;
   package App_Wrapper
      renames Interface_Layer.Wrappers.Application;
   package Infrastructure_Registry renames Reactive.Infrastructure_Registry;
   package Traveller_Registry      renames Reactive.Traveller_Registry;
   package Failure_Callback_Pkg    renames Scheduling.Remote.Callback.Failure;
   package Success_Callback_Pkg    renames Scheduling.Remote.Callback.Success;
   package Scheduler               renames Scheduling.Scheduler;
   package Callback_Pair_Pkg
      renames Shared.Callback_Pair;
   package SU                      renames Ada.Strings.Unbounded;

   use Types.Recipient_Type_Pkg;
   use Reactive.Infrastructure.Lane;

   function Get_Instance (
      Infrastructure_Registry :
         access Reactive.Infrastructure_Registry.Object'Class := null;
      Traffic_Light_Registry :
         access Reactive.Traffic_Light_Registry.Object'Class := null;
      Traveller_Registry :
         access Reactive.Traveller_Registry.Object'Class := null;
      Host_Registry :
         access Reactive.Host_Registry.Object'Class := null;
      App_Wrapper_Factory :
         access Interface_Layer.Wrappers.Application.Abstract_Factory.Object'Class
         :=null;
      Stub : access Remote_Stub.Object'Class := null;
      Traveller_Utils :
         access Active.Traveller.Utils.Object'Class := null)
      return District.Reference
   is
      AWF_Concrete_Ref
         : Interface_Layer.Wrappers.Application.Concrete_Factory.Reference;
   begin
      if Instance = null then
         Instance := new District.Object;
      end if;

      if Infrastructure_Registry = null then
         Instance.Infrastructure_Registry
           := Reactive.Infrastructure_Registry.Get_Instance;
      else
         Instance.Infrastructure_Registry := Infrastructure_Registry;
      end if;

      if Traffic_Light_Registry = null then
         Instance.Traffic_Light_Registry
           := Reactive.Traffic_Light_Registry.Get_Instance;
      else
         Instance.Traffic_Light_Registry := Traffic_Light_Registry;
      end if;

      if Traveller_Registry = null then
         Instance.Traveller_Registry
           := Reactive.Traveller_Registry.Get_Instance;
      else
         Instance.Traveller_Registry := Traveller_Registry;
      end if;

      if Host_Registry = null then
         Instance.Host_Registry
           := Reactive.Host_Registry.Get_Instance;
      else
         Instance.Host_Registry := Host_Registry;
      end if;

      if App_Wrapper_Factory = null then
         AWF_Concrete_Ref := new
            Interface_Layer.Wrappers.Application.Concrete_Factory.Object;
         Instance.App_Wrapper_Factory := AWF_Concrete_Ref;
      else
         Instance.App_Wrapper_Factory := App_Wrapper_Factory;
      end if;

      if Stub = null then
         Instance.Stub := Remote_Stub.Create;
      else
         Instance.Stub := Stub;
      end if;

      if Traveller_Utils = null then
         Instance.Traveller_Utils :=
            Active.Traveller.Utils.Get_Instance (Instance);
      else
         Instance.Traveller_Utils := Traveller_Utils;
      end if;

      return Instance;
   end Get_Instance;

   function Contains_Infrastructure (This              : in District.Object;
                                     Infrastructure_Id : in Infra_Id)
   return Boolean is
   begin
      return This.Infrastructure_Registry.Contains_Infrastructure (
         Infrastructure_Id);
   end Contains_Infrastructure;

   function Contains_Treadable (This         : in District.Object;
                                Treadable_Id : in Infra_Id)
   return Boolean is
   begin
      return This.Infrastructure_Registry.Contains_Treadable (
         Treadable_Id);
   end Contains_Treadable;

   function Contains_Host (This    : in District.Object;
                           Host_Id : in Infra_Id)
   return Boolean is
   begin
      return This.Host_Registry.Contains_Host (Host_Id);
   end Contains_Host;

   function Contains_Traveller (This         : in District.Object;
                                Traveller_Id : in Agent.Agent_Id)
   return Boolean is
   begin
      return This.Traveller_Registry.Contains_Traveller (Traveller_Id);
   end Contains_Traveller;

   function Contains_Traffic_Light (This         : in District.Object;
                                    Traffic_Light_Id : in Agent.Agent_Id)
   return Boolean is
   begin
      return This.Traffic_Light_Registry.Contains_Traffic_Light (
         Traffic_Light_Id);
   end Contains_Traffic_Light;

   function Dump (This : in District.Object) return G_JSON.JSON_Value
   is
      Reactive_JSON       : G_JSON.JSON_Value;
      Hosts_JSON          : G_JSON.JSON_Value;
      Traffic_Light_JSON  : G_JSON.JSON_Value;
   begin
      Reactive_JSON := This.Infrastructure_Registry.Dump;
      Hosts_JSON := This.Host_Registry.Dump;
      Reactive_JSON.Set_Field ("facilities", Hosts_JSON);
      return Reactive_JSON;
   end Dump;

   function Dump_Travellers (This : in District.Object)
   return G_JSON.JSON_Value is
   begin
      return This.Traveller_Registry.Dump;
   end Dump_Travellers;

   function Dump_Traffic_Lights (This : in District.Object)
   return G_JSON.JSON_Value is
   begin
      return This.Traffic_Light_Registry.Dump;
   end;

   function Find_Infrastructure_By_Id (This              : in District.Object;
                                       Infrastructure_Id : in Infra_Id)
      return Infrastructure.Reference is
   begin
      return This.Infrastructure_Registry
                  .Find_Infrastructure_By_Id (Infrastructure_Id);
   end Find_Infrastructure_By_Id;

   function Find_Treadable_By_Id (This         : in District.Object;
                                  Treadable_Id : in Infra_Id)
   return Treadable.Reference is
   begin
      return This.Infrastructure_Registry
                 .Find_Treadable_By_Id (Treadable_Id);
   end Find_Treadable_By_Id;

   function Find_Intersection_By_Id (This            : in District.Object;
                                     Intersection_Id : in Infra_Id)
      return Intersection.Reference is
   begin
      return This.Infrastructure_Registry
                  .Find_Intersection_By_Id (Intersection_Id);
   end Find_Intersection_By_Id;

   function Find_Street_By_Id (This      : in District.Object;
                               Street_Id : in Infra_Id)
      return Street.Reference is
   begin
      return This.Infrastructure_Registry.Find_Street_By_Id (Street_Id);
   end Find_Street_By_Id;

   function Find_Way_By_Id (This   : in District.Object;
                            Way_Id : in Infra_Id)
      return Way.Reference is
   begin
      return This.Infrastructure_Registry.Find_Way_By_Id (Way_Id);
   end Find_Way_By_Id;

   function Find_Roadway_By_Id (This       : in District.Object;
                                Roadway_Id : in Infra_Id)
      return Roadway.Reference is
   begin
      return This.Infrastructure_Registry.Find_Roadway_By_Id (Roadway_Id);
   end Find_Roadway_By_Id;

   function Find_Footway_By_Id (This       : in District.Object;
                                Footway_Id : in Infra_Id)
      return Footway.Reference is
   begin
      return This.Infrastructure_Registry.Find_Footway_By_Id (Footway_Id);
   end Find_Footway_By_Id;

   function Find_Bikeway_By_Id (This       : in District.Object;
                                Bikeway_Id : in Infra_Id)
      return Bikeway.Reference is
   begin
      return This.Infrastructure_Registry.Find_Bikeway_By_Id (Bikeway_Id);
   end Find_Bikeway_By_Id;

   function Find_Lane_By_Id (This    : in District.Object;
                             Lane_Id : in Infra_Id)
   return Lane.Reference
   is
   begin
      return This.Infrastructure_Registry.Find_Lane_By_Id (Lane_Id);
   end Find_Lane_By_Id;

   function Find_Stretch_By_Id (This       : in District.Object;
                                Stretch_Id : in Infra_Id)
      return Stretch.Reference is
   begin
      return This.Infrastructure_Registry.Find_Stretch_By_Id (Stretch_Id);
   end Find_Stretch_By_Id;

   function Find_Agent_By_Id (This     : in District.Object;
                              Agent_Id : in Agent.Agent_Id)
   return Agent.Reference is
   begin
     if This.Contains_Traveller (Agent_Id) then
       return Agent.Reference (This.Find_Traveller_By_Id (Agent_Id));
     end if;
     return Agent.Reference (This.Find_Traffic_Light_By_Id (Agent_Id));
   end Find_Agent_By_Id;

   function Find_Traveller_By_Id (This         : in District.Object;
                                  Traveller_Id : in Agent.Agent_Id)
      return Traveller.Reference is
   begin
      return This.Traveller_Registry.Find_Traveller_By_Id (Traveller_Id);
   end Find_Traveller_By_Id;

   function Find_Traffic_Light_By_Id (
      This             : in District.Object;
      Traffic_Light_Id : in Agent.Agent_Id)
   return Traffic_Light.Reference is
   begin
      return This.Traffic_Light_Registry.Find_Traffic_Light_By_Id (
         Traffic_Light_Id);
   end Find_Traffic_Light_By_Id;

   function Find_Host_By_Id (This    : in District.Object;
                             Host_Id : in Infra_Id)
   return Host_Pkg.Reference is
   begin
      return This.Host_Registry.Find_Host_By_Id (Host_Id);
   end Find_Host_By_Id;

   procedure Add_Intersection (
      This           : in out District.Object;
      Infrastructure :
         aliased in out Reactive.Infrastructure.Intersection.Object'Class;
      Added          :    out Boolean) is
   begin
      This.Infrastructure_Registry.Add_Intersection (
         Infrastructure => Infrastructure, Added => Added);
   end Add_Intersection;

   procedure Add_Street (
      This      : in out District.Object;
      SR_Street : in out SR_Street_Pkg.Shared_Reference;
      Added     :    out Boolean) is
   begin
      This.Infrastructure_Registry.Add_Street (
         SR_Street => SR_Street, Added => Added);
   end Add_Street;

   procedure Add_Roadway (
      This           : in out District.Object;
      Infrastructure :
         aliased in out Reactive.Infrastructure.Way.Roadway.Reference;
      Added          :    out Boolean) is
   begin
      This.Infrastructure_Registry.Add_Roadway (
         Infrastructure => Infrastructure, Added => Added);
   end Add_Roadway;

   procedure Add_Footway (
      This           : in out District.Object;
      Infrastructure :
         aliased in out Reactive.Infrastructure.Way.Footway.Reference;
      Added          :    out Boolean) is
   begin
      This.Infrastructure_Registry.Add_Footway (
         Infrastructure => Infrastructure, Added => Added);
   end Add_Footway;

   procedure Add_Bikeway (
      This           : in out District.Object;
      Infrastructure :
         aliased in out Reactive.Infrastructure.Way.Bikeway.Reference;
      Added          :    out Boolean) is
   begin
      This.Infrastructure_Registry.Add_Bikeway (
         Infrastructure => Infrastructure, Added => Added);
   end Add_Bikeway;

   procedure Add_Lane (
      This           : in out District.Object;
      Infrastructure :
         aliased in out Reactive.Infrastructure.Lane.Reference;
      Added          :    out Boolean) is
   begin
      This.Infrastructure_Registry.Add_Lane (
         Infrastructure => Infrastructure, Added => Added);
   end Add_Lane;

   procedure Add_Stretch (
      This           : in out District.Object;
      Infrastructure :
         aliased in out Reactive.Infrastructure.Stretch.Reference;
      Added          :    out Boolean) is
   begin
      This.Infrastructure_Registry.Add_Stretch (
         Infrastructure => Infrastructure, Added => Added);
   end Add_Stretch;

   procedure Add_Traveller (
      This      : in out District.Object;
      Traveller : in out Active.Traveller.Reference;
      Added     :    out Boolean) is
   begin
      This.Traveller_Registry.Add_Traveller  (Traveller => Traveller,
                                             Added     => Added);
   end Add_Traveller;

   procedure Add_Traffic_Light (
      This          : in out District.Object;
      Traffic_Light : aliased in out Active.Traffic_Light.Object'Class;
      Added         : out Boolean) is
   begin
      This.Traffic_Light_Registry.Add_Traffic_Light (
         Traffic_Light => Traffic_Light,
         Added         => Added);
   end Add_Traffic_Light;

   not overriding
   procedure Add_Host (
      This     :         in out District.Object;
      Host_Ref : aliased in out Host_Pkg.Object'Class;
      Added    :            out Boolean) is
   begin
      This.Host_Registry.Add_Host (
         Host  => Host_Ref,
         Added => Added);
   end Add_Host;

   procedure Remove_Traveller (
      This         : in out District.Object;
      Traveller_Id : in     Agent.Agent_Id;
      Removed      :    out Boolean) is
   begin
      This.Traveller_Registry.Remove_Traveller (Traveller_Id => Traveller_Id,
                                                Removed => Removed);
   end Remove_Traveller;

   procedure Schedule (
      This      : in out District.Object;
      Traveller : aliased in out Active.Traveller.Reference)
   is
      Scheduled_Entity   : Active.Agent.Reference
         := Active.Agent.Reference (Traveller);
      Deferred_To        : Float := Traveller.Get_Scheduled_For;
      Has_Been_Scheduled : Boolean := FALSE;
   begin
      Scheduler.Instance.Schedule (
         Scheduled_Entity, Deferred_To, Has_Been_Scheduled);
   end Schedule;

   procedure Schedule (
      This          : in out District.Object;
      Traffic_Light : aliased in out Active.Traffic_Light.Reference;
      Offset        :        Natural)
   is
      Scheduled_Entity  : Active.Agent.Reference
                  := Active.Agent.Reference (Traffic_Light);
     Deferred_To        : Float := Float (Traffic_Light.Get_Period + Offset);
     Has_Been_Scheduled : Boolean := FALSE;
   begin
      Scheduler.Instance.Schedule (
         Scheduled_Entity, Deferred_To, Has_Been_Scheduled);
   end Schedule;

   procedure Try_To_Tread_Infrastructure (
      This         : in out District.Object;
      Traveller_Id : in     Agent.Agent_Id;
      Treadable_Id : in     Infra_Id;
      Advanced     :    out Boolean)
   is
      procedure Free is new  Ada.Unchecked_Deallocation (
         App_Wrapper.Object'Class, App_Wrapper.Reference);
      Traveller_Ref  : Active.Traveller.Reference :=
         This.Find_Traveller_By_Id (Traveller_Id);
      Local_Move     : Boolean;
      Infrastructure : Reactive.Treadable.Reference;
      Removed        : Boolean := FALSE;
      T_Wrapper      : App_Wrapper.Reference; -- Traveller wrapper
      Recipient      : Recipient_Type;
   begin
      Local_Move := This.Contains_Treadable (Treadable_Id);
   -- Check if infrastructure is in this district
      if Local_Move then

      -- If so, tread infrastructure
         Infrastructure := This.Find_Treadable_By_Id (Treadable_Id);

         Infrastructure.Tread (Traveller_Id, Advanced);


         if Advanced then
            This.Traveller_Utils.Set_Position (
               Traveller_Id, Treadable_Id);
         -- Make the traveller consume a step
            This.Traveller_Utils.Consume_Step (Traveller_Id);
         -- Notify that a traveller trod a stretch
            T_Wrapper :=
               This.App_Wrapper_Factory.Create_Wrapper (Traveller_Ref);
            Recipient.Id := Treadable_Id;
            Recipient.Sort := Types.TREADABLE;
            This.Stub.Async_Request (
               T_Wrapper, Types.TREAD, Recipient, Traveller_Id);
            Free (T_Wrapper);
         else
         -- Retry at next tick: treadable has not been trod
            This.Traveller_Utils.Defer (Traveller_Id, Retry_Action => TRUE);
         end if;

         return;

      end if;


   -- Otherwise, try to enter infrastructure in other district
      T_Wrapper := This.App_Wrapper_Factory.Create_Wrapper (Traveller_Ref);

      Recipient.Id := Treadable_Id;
      Recipient.Sort := Types.TREADABLE;

      declare
         Success : Success_Callback_Pkg.Reference
            := Success_Callback_Pkg.Tread.Create (
               Traveller_Id, Treadable_Id, This'Unchecked_Access);
         Failure : Failure_Callback_Pkg.Reference
            := Failure_Callback_Pkg.Tread.Create (
               Traveller_Id, This.Traveller_Utils);
         Pair    : Callback_Pair_Pkg.Object
            := Callback_Pair_Pkg.Create (Success, Failure);
      begin
      -- Perform asynchronous request to enter district on the other side
      -- pass rendezvous down
         This.Stub.Enter (T_Wrapper, Pair, Recipient, Traveller_Id);
      end;

   -- Set Advanced to False to avoid the Traveller to be automatically
   --+ re-scheduled
      Advanced := False;

      Free (T_Wrapper);
   end Try_To_Tread_Infrastructure;

end Reactive.District;
