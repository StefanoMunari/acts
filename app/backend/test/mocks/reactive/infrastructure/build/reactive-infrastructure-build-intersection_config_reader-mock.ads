with Ada.Containers.Doubly_Linked_Lists;

with Shared.Infra_Id_List;

package Reactive.Infrastructure.Build.Intersection_Config_Reader.Mock is

   package Infra_Id_List renames Shared.Infra_Id_List;

   type Object is
     new Intersection_Config_Reader.Object
   with private;
   type Reference is access all Intersection_Config_Reader.Mock.Object'Class;

   function Create return Intersection_Config_Reader.Mock.Reference;

   overriding
   procedure Set_Builder (
      This    :    out Intersection_Config_Reader.Mock.Object;
      Builder : access Intersection_Builder_Pkg.Object'Class := null);

   overriding
   function Read (This        : in out Intersection_Config_Reader.Mock.Object;
                  Intersection_Json : in     G_JSON.JSON_Value) return Infra_Id;

   not overriding
   function Get_Set_Builder_Called (
      This : in out Intersection_Config_Reader.Mock.Object)
   return Boolean;

   not overriding
   procedure Set_Return_Value_For_Read (
      This         : in out Intersection_Config_Reader.Mock.Object;
      Return_Value : in     Infra_Id);

   not overriding
   function Get_Read_Called (
      This         : in out Intersection_Config_Reader.Mock.Object)
   return Boolean;

private
   type Mock_Values_Collection is record
      Set_Builder_Called : Boolean := FALSE;
      Read_Called        : Boolean := FALSE;
   end record;

   type Return_Values_Collection is record
      Read : Infra_Id_List.List;
      Read_Existence : Boolean := FALSE;
   end record;

   type Object is
     new Intersection_Config_Reader.Object
   with record
      Mock_Values   : Mock_Values_Collection;
      Return_Values : Return_Values_Collection;
   end record;

end Reactive.Infrastructure.Build.Intersection_Config_Reader.Mock;
