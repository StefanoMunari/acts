with Active.Agent;

with Shared.Infra_Id_Set;
with Shared.Direction;

package Reactive.Infrastructure.Street_Related_Infrastructure.Mock is

   package Agent renames Active.Agent;
   package Direction renames Shared.Direction;
   package Infra_Id_Set renames Shared.Infra_Id_Set;

   type Object (<>) is
     new Street_Related_Infrastructure.Object
   with private;
   type Reference is access all Street_Related_Infrastructure.Mock.Object'Class;

   function Create return Street_Related_Infrastructure.Mock.Reference;

   overriding
   function Get_Id (This : in Street_Related_Infrastructure.Mock.Object)
   return Infra_Id;

   overriding
   function "=" (This, Outher : in Street_Related_Infrastructure.Mock.Object) return Boolean;

   overriding
   function Find_Street (This : in Street_Related_Infrastructure.Mock.Object)
                         return Infra_Id;

   overriding
   function Is_Contained_By (
      This         : in Street_Related_Infrastructure.Mock.Object;
      Container_Id : in Infra_Id) return Boolean;

   overriding
   function Dump (This : Street_Related_Infrastructure.Mock.Object)
   return G_JSON.JSON_Value;

   not overriding
   procedure Set_Id (
      This         : in out Street_Related_Infrastructure.Mock.Object;
      Return_Value : in Infra_Id);

   not overriding
   procedure Set_Return_Value_For_Equality_Operator (
      This         : in out Street_Related_Infrastructure.Mock.Object;
      Return_Value : in Boolean);

   not overriding
   procedure Set_Return_Value_For_Find_Street (
      This         : in out Street_Related_Infrastructure.Mock.Object;
      Return_Value : in Infra_Id);

   not overriding
   procedure Set_Return_Value_For_Is_Contained_By (
      This         : in out Street_Related_Infrastructure.Mock.Object;
      Return_Value : in     Boolean);

   not overriding
   procedure Set_Return_Value_For_Dump (
      This         : in out Street_Related_Infrastructure.Mock.Object;
      Return_Value : in     G_JSON.JSON_Value);

private
   type Mock_Values_Collection is record
      Id : Infra_Id;
      Id_Existence : Boolean := FALSE;
   end record;

   type Return_Values_Collection is record
      Equality_Operator : Boolean;
      Equality_Operator_Existence : Boolean := FALSE;
      Find_Street : Infra_Id;
      Find_Street_Existence : Boolean := FALSE;
      Is_Contained_By : Boolean;
      Is_Contained_By_Existence : Boolean := FALSE;
      Dump : G_JSON.JSON_Value;
      Dump_Existence : Boolean := FALSE;
   end record;

   type Object is
     new Street_Related_Infrastructure.Object
   with record
      Mock_Values : Mock_Values_Collection;
      Return_Values : Return_Values_Collection;
   end record;

end Reactive.Infrastructure.Street_Related_Infrastructure.Mock;
