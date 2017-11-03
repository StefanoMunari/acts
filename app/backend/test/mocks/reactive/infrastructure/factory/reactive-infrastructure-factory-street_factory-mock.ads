with Ada.Containers.Doubly_Linked_Lists;

with Shared.Infra_Id_List;

package Reactive.Infrastructure.Factory.Street_Factory.Mock is

   package Infra_Id_List renames Shared.Infra_Id_List;

   type Object is
     new Street_Factory.Object
   with private;
   type Reference is access all Street_Factory.Mock.Object'Class;

   package Stretch_List is
   new Ada.Containers.Doubly_Linked_Lists (
      Element_Type => Stretch.Reference,
      "="          => Stretch."=");

   package Lane_List is
   new Ada.Containers.Doubly_Linked_Lists (
      Element_Type => Lane.Reference,
      "="          => Lane."=");

   function Create return Infrastructure.Factory.Street_Factory.Mock.Reference;

   overriding
   function Decorate_Stretch (
      This        : in out Street_Factory.Mock.Object;
      Stretch_Ref : in     Reactive.Infrastructure.Stretch.Reference;
      Decoration  : in     G_JSON.JSON_Value)
   return Stretch.Reference;

   overriding
   function Decorate_Lane (
      This       : in out Street_Factory.Mock.Object;
      Lane       : in     Reactive.Infrastructure.Lane.Reference;
      Decoration : in     G_JSON.JSON_Value)
   return Reactive.Infrastructure.Lane.Reference;

   overriding
   function Create_Stretch (This : in out Street_Factory.Mock.Object)
      return Stretch.Reference;

   overriding
   procedure Set_Stretch_Id (This : in out Street_Factory.Mock.Object;
                             Id   : in     Infra_Id);

   overriding
   procedure Set_Stretch_Size (This : in out Street_Factory.Mock.Object;
                               Size : in     Natural);

   overriding
   function Create_Lane (This : in out Street_Factory.Mock.Object)
      return Lane.Reference;

   overriding
   procedure Set_Lane_Id (This : in out Street_Factory.Mock.Object;
                          Id   : in     Infra_Id);

   overriding
   procedure Set_Lane_Direction (
      This      : in out Street_Factory.Mock.Object;
      Direction : in     Shared.Direction.Straight);

   overriding
   procedure Set_Lane_Stretches (This      : in out Street_Factory.Mock.Object;
                                 Stretches : in     Infra_Id_List.List);

   not overriding
   procedure Set_Return_Value_Decorate_Stretch (
      This         : in out Street_Factory.Mock.Object;
      Return_Value : in     Stretch.Reference);

   not overriding
   procedure Set_Return_Value_Decorate_Lane (
      This         : in out Street_Factory.Mock.Object;
      Return_Value : in     Lane.Reference);

   not overriding
   procedure Set_Return_Value_Create_Stretch (
      This         : in out Street_Factory.Mock.Object;
      Return_Value : in     Stretch.Reference);

   not overriding
   procedure Set_Return_Value_Create_Lane (
      This         : in out Street_Factory.Mock.Object;
      Return_Value : in     Lane.Reference);

private
   type Mock_Values_Collection is record
      Set_Stretch_Id_Called : Boolean := FALSE;
      Set_Stretch_Size_Called : Boolean := FALSE;
      Set_Lane_Id_Called : Boolean := FALSE;
      Set_Lane_Direction_Called : Boolean := FALSE;
      Set_Lane_Stretches_Called : Boolean := FALSE;
   end record;

   type Return_Values_Collection is record
      Decorate_Stretch : Stretch.Reference;
      Decorate_Stretch_Existence : Boolean := FALSE;
      Decorate_Lane    : Lane.Reference;
      Decorate_Lane_Existence : Boolean := FALSE;
      Create_Stretch   : Stretch_List.List;
      Create_Lane      : Lane_List.List;
   end record;

   type Object is
     new Street_Factory.Object
   with record
      Mock_Values   : Mock_Values_Collection;
      Return_Values : Return_Values_Collection;
   end record;

end Reactive.Infrastructure.Factory.Street_Factory.Mock;
