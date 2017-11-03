-- core
with Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;

with Active.Traveller;
with Active.Travel;

with Interface_Layer.Remote.Query_Decoder;
with Interface_Layer.Service.Pipelines.Handler.Concrete_Factory;
with Interface_Layer.Utils.Remote_Exceptions;
with Interface_Layer.Utils.Types;
with Interface_Layer.Wrappers.Application.Concrete_Factory;

with Reactive.District;

with Scheduling.System;

with Interface_Layer.System;

package body Interface_Layer.Remote.Skeleton is

   package District renames Reactive.District;
--   package Scheduler renames Scheduling.Scheduler;
   package Exc renames Interface_Layer.Utils.Remote_Exceptions;
   package SU renames Ada.Strings.Unbounded;
   package Types renames Interface_Layer.Utils.Types;
   package Concrete_Handler_Factory
      renames Interface_Layer.Service.Pipelines.Handler.Concrete_Factory;
   package Concrete_Wrapper_Factory renames App_Wrapper_Pkg.Concrete_Factory;

   function Create (
      Handler_Factory : access Abstract_Handler_Factory.Object'Class := null;
      Wrapper_Factory : access Abstract_Wrapper_Factory.Object'Class := null;
      Dispatcher      : access Query_Dispatcher.Object'Class         := null;
      Space_Master    : access Active.Space_Master.Object'Class      := null)
   return Skeleton.Object is
      This : Skeleton.Object;
   begin

      if Handler_Factory = null then
         This.Handler_Factory := new Concrete_Handler_Factory.Object;
      else
         This.Handler_Factory := Handler_Factory;
      end if;

      if Handler_Factory = null then
         This.Wrapper_Factory := new Concrete_Wrapper_Factory.Object;
      else
         This.Wrapper_Factory := Wrapper_Factory;
      end if;

      if Dispatcher = null then
         This.Dispatcher := Query_Dispatcher.Get_Instance;
      else
         This.Dispatcher := Dispatcher;
      end if;

      if Space_Master = null then
         This.Space_Master := Space_Master_Pkg.Get_Instance;
      else
         This.Space_Master := Space_Master;
      end if;

      return This;
   end Create;

--   procedure Finalize (This : in out Skeleton.Object) is
--      procedure Free is new
--      Ada.Unchecked_Deallocation (
--         Abstract_Handler_Factory.Object'Class,
--         Abstract_Handler_Factory.Reference);
--   begin
--      Free (This.Handler_Factory);
--   end Finalize;

--   procedure Finalize (This_Ref : in out Skeleton.Reference) is
--      procedure Free is new Ada.Unchecked_Deallocation (
--         Skeleton.Object'Class, Skeleton.Reference);
--   begin
--      This_Ref.all.Finalize;
--      Free (This_Ref);
--   end Finalize;

   procedure Enter (This   : in     Skeleton.Object;
                    Entity : in out Interface_Wrapper.Object)
   is separate;

   procedure Query (This    : in     Skeleton.Object;
                    Message : in out Interface_Wrapper.Object)
   is separate;

   procedure Snapshot (This    : in     Skeleton.Object;
                       Request : in out Interface_Wrapper.Object) is
   begin
      null;
   -- Print the received message to console
   end Snapshot;

   procedure Start (This    : in     Skeleton.Object;
                   Request : in out Interface_Wrapper.Object)
   is separate;

   procedure Shutdown (This    : in     Skeleton.Object;
                       Request : in out Interface_Wrapper.Object)
   is separate;

end Interface_Layer.Remote.Skeleton;
