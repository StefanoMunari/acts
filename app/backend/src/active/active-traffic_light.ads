------------------------------------------------------------------------------
-- @author  <gianmarco.midena@gmail.com>
-- @author  <stefanomunari.sm@gmail.com>
-- @author  <valle.sebastiano93@gmail.com>
-- @context application-backend::active-traffic_light
-- @purpose Represents a traffic light
-- @interface Act (Object):
--              triggers the change of color
-- @dependencies application-backend::active-agent
--               application-backend::reactive
-- @details Implementation for Agent. Most of the methods are not listed in the
--          @interface API since they're self-explanatory
------------------------------------------------------------------------------

-- gnatcoll libs
with GNATCOLL.JSON;

with Active.Agent;

with Interface_Layer.Remote.Stub;
with Interface_Layer.Wrappers.Application;
with Interface_Layer.Wrappers.Application.Abstract_Factory;

with Reactive;

package Active.Traffic_Light is

   package G_JSON       renames GNATCOLL.JSON;
   package Agent        renames Active.Agent;
   package Stub_Pkg     renames Interface_Layer.Remote.Stub;
   package App_Wrapper  renames Interface_Layer.Wrappers.Application;

   use Reactive.Infra_Id_Type;

   type Object is new Agent.Object with private;
   type Reference is access all Traffic_Light.Object'Class;

   type Traffic_Light_Color is (RED,
                                GREEN);

   overriding
   procedure Act (This : in out Traffic_Light.Object);

   not overriding
   function Create (
      Id                  : in Agent.Agent_Id;
      Starting_Color      : in Traffic_Light_Color;
      Period              : in Natural;
      App_Wrapper_Factory :
         access App_Wrapper.Abstract_Factory.Object'Class := null;
      Stub                : access Stub_Pkg.Object'Class := null)
   return Traffic_Light.Reference;

   not overriding
   function Get_Color (This : in Traffic_Light.Object)
   return Traffic_Light_Color;

   overriding
   function Get_Id (This : in Traffic_Light.Object) return Agent.Agent_Id;

   not overriding
   function Get_Period (This : in Traffic_Light.Object) return Natural;

   not overriding
   function Is_Green (This : in Traffic_Light.Object) return Boolean;

   not overriding
   procedure Set_Intersection_Id (
      This            : in out Traffic_Light.Object;
      Intersection_Id : in     Infra_Id);

   not overriding
   function Dump (This : in Traffic_Light.Object) return G_JSON.JSON_Value;

-- JSON FIELDS CONSTANTS
   function Id_Field     return String is ("id");
   function Color_Field  return String is ("color");
   function Period_Field return String is ("period");

private
   type Object is new Agent.Object with record
      Color        : Traffic_Light_Color;
      Id           : Agent.Agent_Id;
      Period       : Natural;
      Intersection : Infra_Id;
      W_Factory    : access App_Wrapper.Abstract_Factory.Object'Class;
      Stub         : access Stub_Pkg.Object'Class;
   end record;

   not overriding
   procedure Change_Color (This : in out Traffic_Light.Object;
                           New_Color : Traffic_Light_Color);

end Active.Traffic_Light;
