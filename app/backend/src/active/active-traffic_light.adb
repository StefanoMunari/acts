with Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;

with Interface_Layer.Utils.Types;
with Interface_Layer.Wrappers.Application.Concrete_Factory;

with Reactive.District;

package body Active.Traffic_Light is

   package Types        renames Interface_Layer.Utils.Types;
   package District     renames Reactive.District;
   package SU           renames Ada.Strings.Unbounded;

   use Types.Recipient_Type_Pkg;

   procedure Act (This : in out Traffic_Light.Object)
   is
      District_Ref       : District.Reference := District.Get_Instance;
      This_Traffic_Light : aliased Traffic_Light.Reference;
      Traffic_Light_Id   : Agent.Agent_Id := This.Get_Id;
   begin
      -- Change color
      if This.Color = RED then
         This.Change_Color(GREEN);
      else
         This.Change_Color(RED);
      end if;

      -- Find this traffic light by id
      This_Traffic_Light := District_Ref.Find_Traffic_Light_By_Id (
         Traffic_Light_Id);
      District_Ref.Schedule (This_Traffic_Light, 0);
   end Act;

   function Create (
      Id                  : in Agent.Agent_Id;
      Starting_Color      : in Traffic_Light_Color;
      Period              : in Natural;
      App_Wrapper_Factory :
         access App_Wrapper.Abstract_Factory.Object'Class := null;
      Stub                : access Stub_Pkg.Object'Class := null)
   return Traffic_Light.Reference is
      New_Traffic_Light : aliased Traffic_Light.Reference
         := new Traffic_Light.Object;
      District_Ref : District.Reference := District.Get_Instance;
      Added : Boolean := False;
   begin
      New_Traffic_Light.Color  := Starting_Color;
      New_Traffic_Light.Period := Period;
      New_Traffic_Light.Id     := Id;

      -- Register traffic light in the district
      District_Ref.Add_Traffic_Light (New_Traffic_Light.all, Added);

      if App_Wrapper_Factory = null then
         New_Traffic_Light.W_Factory :=
            new App_Wrapper.Concrete_Factory.Object;
      else
         New_Traffic_Light.W_Factory := App_Wrapper_Factory;
      end if;
      if Stub = null then
         New_Traffic_Light.Stub := Stub_Pkg.Create;
      else
         New_Traffic_Light.Stub := Stub;
      end if;

      return New_Traffic_Light;
   end Create;

   function Get_Color (This : in Traffic_Light.Object)
      return Traffic_Light_Color is
   begin
      return This.Color;
   end Get_Color;

   function Get_Id (This : in Traffic_Light.Object) return Agent.Agent_Id is
   begin
      return This.Id;
   end Get_Id;

   function Get_Period (This : in Traffic_Light.Object) return Natural is
   begin
      return This.Period;
   end Get_Period;

   function Is_Green (This : in Traffic_Light.Object)
                      return Boolean
   is (This.Color = GREEN);

   not overriding
   procedure Set_Intersection_Id (
      This            : in out Traffic_Light.Object;
      Intersection_Id : in     Infra_Id) is
   begin
      This.Intersection := Intersection_Id;
   end Set_Intersection_Id;

   procedure Change_Color (
      This      : in out Traffic_Light.Object;
      New_Color : Traffic_Light_Color)
   is
      procedure Free is new  Ada.Unchecked_Deallocation (
         App_Wrapper.Object'Class, App_Wrapper.Reference);
      T_Wrapper      : App_Wrapper.Reference; -- Traveller wrapper
      Recipient      : Recipient_Type;
   begin
      This.Color := New_Color;
      T_Wrapper :=
         This.W_Factory.Create_Wrapper (
            SU.To_Unbounded_String (Traffic_Light_Color'Image (This.Color)));
      Recipient.Id := This.Intersection;
      Recipient.Sort := Types.TREADABLE;
      This.Stub.Async_Request_Other (
         T_Wrapper, Types.TRAFFIC_LIGHT, Recipient, This.Id);
      Free (T_Wrapper);
   end Change_Color;

   function Dump (This : in Traffic_Light.Object) return G_JSON.JSON_Value
   is
      JSON : G_JSON.JSON_Value := G_JSON.Create_Object;
   begin
      JSON.Set_Field (Id_Field, This.Id);
      JSON.Set_Field (Color_Field, Traffic_Light_Color'Image (This.Color));
      JSON.Set_Field (Period_Field, This.Period);
      return JSON;
   end Dump;

end Active.Traffic_Light;
