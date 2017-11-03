with Ada.Containers.Ordered_Maps;

with GNATCOLL.JSON;

with Reactive.Infrastructure.Building.Host;

package Reactive.Host_Registry is

-- libs
   package G_JSON       renames GNATCOLL.JSON;
-- reactive
   package Host_Pkg renames Reactive.Infrastructure.Building.Host;
   use Reactive.Infra_Id_Type;

   package Host_By_Id is
     new Ada.Containers.Ordered_Maps (
        Key_Type     => Infra_Id,
        Element_Type => Host_Pkg.Reference,
        "<"          => "<",
        "="          => Host_Pkg."=");

   type Object (<>) is tagged limited private;
   type Reference is access all Host_Registry.Object'Class;

   function Get_Instance return Host_Registry.Reference;

   not overriding
   function Contains_Host (
      This    : in Host_Registry.Object;
      Host_Id : in Infra_Id)
   return Boolean;

   not overriding
   function Find_Host_By_Id (
      This    : in Host_Registry.Object;
      Host_Id : in Infra_Id)
   return Host_Pkg.Reference;

   not overriding
   procedure Add_Host (
      This  :         in out Host_Registry.Object;
      Host  : aliased in out Host_Pkg.Object'Class;
      Added :            out Boolean);

   not overriding
   procedure Remove_Host (
      This    : in out Host_Registry.Object;
      Host_Id : in     Infra_Id;
      Removed :    out Boolean);

   not overriding
   function Dump (This : in Host_Registry.Object)
   return G_JSON.JSON_Value;

   protected type Population is

      not overriding
      function Contains_Host (Host_Id : in Infra_Id)
      return Boolean;

      not overriding
      function Find_Host_By_Id (Host_Id : in Infra_Id)
      return Host_Pkg.Reference;

      not overriding
      procedure Add_Host (
         Host  : aliased in out Host_Pkg.Object'Class;
         Added :            out Boolean);

      not overriding
      procedure Remove_Host (Host_Id : in     Infra_Id;
                             Removed :    out Boolean);

      not overriding
      procedure Clear;

      function Dump
      return G_JSON.JSON_Value;

   private
      Population : Host_By_Id.Map;
   end Population;

private
   type Object is tagged limited record
      Population : access Host_Registry.Population
         := new Host_Registry.Population;
   end record;

   Instance : Host_Registry.Reference;

end Reactive.Host_Registry;
