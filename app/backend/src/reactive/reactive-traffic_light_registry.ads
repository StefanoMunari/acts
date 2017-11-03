with Ada.Containers.Ordered_Maps;

with GNATCOLL.JSON;

with Active.Agent;
with Active.Traffic_Light;

package Reactive.Traffic_Light_Registry is

-- libs
   package G_JSON       renames GNATCOLL.JSON;
-- active
   package Agent renames Active.Agent;
   package Traffic_Light renames Active.Traffic_Light;

   package Traffic_Light_By_Id is
     new Ada.Containers.Ordered_Maps (
        Key_Type     => Agent.Agent_Id,
        Element_Type => Traffic_Light.Reference,
        "<"          => Agent."<",
        "="          => Traffic_Light."=");

   type Object (<>) is tagged limited private;
   type Reference is access all Traffic_Light_Registry.Object'Class;

   function Get_Instance return Traffic_Light_Registry.Reference;

   function Contains_Traffic_Light (
      This             : in Traffic_Light_Registry.Object;
      Traffic_Light_Id : in Agent.Agent_Id)
   return Boolean;

   function Find_Traffic_Light_By_Id (
      This             : in Traffic_Light_Registry.Object;
      Traffic_Light_Id : in Agent.Agent_Id)
   return Traffic_Light.Reference;

   procedure Add_Traffic_Light (
      This          :         in out Traffic_Light_Registry.Object;
      Traffic_Light : aliased in out Active.Traffic_Light.Object'Class;
      Added         :            out Boolean);

   procedure Remove_Traffic_Light (
      This             : in out Traffic_Light_Registry.Object;
      Traffic_Light_Id : in     Agent.Agent_Id;
      Removed          :    out Boolean);

   function Dump (This : in Traffic_Light_Registry.Object)
   return G_JSON.JSON_Value;

   protected type Population is

      not overriding
      function Contains_Traffic_Light (Traffic_Light_Id : in Agent.Agent_Id)
      return Boolean;

      not overriding
      function Find_Traffic_Light_By_Id (Traffic_Light_Id : in Agent.Agent_Id)
      return Traffic_Light.Reference;

      not overriding
      procedure Add_Traffic_Light (
         Traffic_Light : aliased in out Active.Traffic_Light.Object'Class;
         Added     : out Boolean);

      not overriding
      procedure Remove_Traffic_Light (Traffic_Light_Id : in     Agent.Agent_Id;
                                      Removed          :    out Boolean);

      not overriding
      procedure Clear;

     function Dump
     return G_JSON.JSON_Value;

   private
      Population : Traffic_Light_By_Id.Map;
   end Population;

private
   type Object is tagged limited record
      Population : access Traffic_Light_Registry.Population
         := new Traffic_Light_Registry.Population;
   end record;

   Instance : Traffic_Light_Registry.Reference;

end Reactive.Traffic_Light_Registry;
