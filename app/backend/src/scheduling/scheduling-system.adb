-- core
with Ada.Environment_Variables;
with Ada.Unchecked_Deallocation;

with Interface_Layer.Remote.Stub;
with Interface_Layer.Wrappers.Application;
with Interface_Layer.Wrappers.Application.Concrete_Factory;

with Scheduling.Build.Config_Reader;
with Scheduling.Scheduler;

package body Scheduling.System is

   package EV          renames Ada.Environment_Variables;
   package Stub_Pkg    renames Interface_Layer.Remote.Stub;
   package App_Wrapper_Pkg
      renames Interface_Layer.Wrappers.Application;
   package Factory_Pkg
      renames Interface_Layer.Wrappers.Application.Concrete_Factory;
   package CR          renames Scheduling.Build.Config_Reader;

   procedure Start
   is
      procedure Free is new  Ada.Unchecked_Deallocation (
         App_Wrapper_Pkg.Object'Class, App_Wrapper_Pkg.Reference);
   -- Reader
      Config_Reader   : CR.Reference := CR.Get_Instance;
   -- City identifier
      City_Id    : String := EV.Value (Name => "CITY_NODE_ID");
   -- Configuration Path of Reactive
      Config_Path     : String :=
         (EV.Value (Name => "CITY_ROOT")) & "/etc/init/" & City_Id & "/";
   -- Get this node identifier
      District_Id     : String := EV.Value (Name => "CITY_DISTRICT_ID");
   -- Names of the configuration files
      Scheduling_File : String := "scheduling" & District_Id & ".conf";
      Result          : Boolean;
      Wrapper_Factory : Factory_Pkg.Reference := new Factory_Pkg.Object;
      Ack_Wrapper     : App_Wrapper_Pkg.Reference;
      Stub            : Stub_Pkg.Reference := Stub_Pkg.Create;
      Running         : Boolean;
   begin
      State.Set_Running (True, Running);
      if not Running then
         Result := Config_Reader.Read_Config (Config_Path & Scheduling_File);
      end if;
      Stub.Boot;

      Free (Ack_Wrapper);
   end Start;

   procedure Dump
   is
      JSON_Dump : G_JSON.JSON_Value;
   begin -- Dump
      JSON_Dump := Scheduler.Instance.Dump;
   -- Dump could be written somewhere
   end Dump;

   function Shutdown return Boolean
   is
   begin
      Scheduler.Instance.Shutdown;
      return True;
   end Shutdown;

   ------------------------------------------------
   --- PROTECTION FROM SUBSEQUENT BOOT REQUESTS
   ------------------------------------------------

   protected body State is

      procedure Set_Running (
         New_State : in     Boolean;
         Old_State :    out Boolean)
      is
      begin
         Old_State := Booted;
         Booted    := New_State;
      end Set_Running;

      function Get_State return Boolean is
      begin
         return Booted;
      end Get_State;

   end State;

end Scheduling.System;