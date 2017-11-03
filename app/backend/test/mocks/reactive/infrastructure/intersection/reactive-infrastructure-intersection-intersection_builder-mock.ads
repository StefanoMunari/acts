with Ada.Containers.Ordered_Maps;

package Reactive.Infrastructure.Intersection.Intersection_Builder.Mock is

   use Shared.Direction;

   type Object is new Intersection_Builder.Object with private;
   type Reference is access all Mock.Object'Class;

   package Infra_Id_To_Cardinal_Map is
      new Ada.Containers.Ordered_Maps (
         Key_Type        => Infra_Id,
         Element_Type    => Shared.Direction.Cardinal,
         "="             => "=",
         "<"             => "<");

   package Agent_Id_To_Cardinal_Map is
      new Ada.Containers.Ordered_Maps (
         Key_Type        => Shared.Direction.Cardinal,
         Element_Type    => Agent.Agent_Id,
         "="             => Agent."=",
         "<"             => "<");

   function Create return Intersection_Builder.Mock.Reference;

   overriding
   procedure With_Street (This      : in out Intersection_Builder.Mock.Object;
                          Street_Id : in     Infra_Id;
                          Stretches : in     Infra_Id_List.List;
                          Direction : in     Shared.Direction.Cardinal);

   overriding
   procedure With_Traffic_Light (
      This           : in out Intersection_Builder.Mock.Object;
      Traffic_Light  : in     Agent.Agent_Id;
      Exit_Direction : in     Direction.Cardinal);

   overriding
   function Get_Result (This : in Intersection_Builder.Mock.Object)
   return Infra_Id;

   not overriding
   procedure Set_Return_Value_For_Get_Result (
      This         : in out Intersection_Builder.Mock.Object;
      Return_Value : in     Infra_Id);

   not overriding
   function Get_With_Street_Called (
      This      : in out Intersection_Builder.Mock.Object;
      Street_Id : in     Infra_Id;
      Direction :        Shared.Direction.Cardinal) return Boolean;

   not overriding
   function Get_With_Traffic_Light_Called (
      This           : in out Intersection_Builder.Mock.Object;
      Traffic_Light  : in     Agent.Agent_Id;
      Exit_Direction : in     Direction.Cardinal) return Boolean;

private
   type Return_Values_Collection is record
      Get_Result : Infra_Id;
      Get_Result_Existence : Boolean := FALSE;
   end record;

   type Mock_Values_Collection is record
      With_Street_Called : Infra_Id_To_Cardinal_Map.Map
         := Infra_Id_To_Cardinal_Map.Empty_Map;
      With_Traffic_Light_Called : Agent_Id_To_Cardinal_Map.Map
         := Agent_Id_To_Cardinal_Map.Empty_Map;
   end record;

   type Object is new Intersection_Builder.Object with record
      Mock_Values   : Mock_Values_Collection;
      Return_Values : Return_Values_Collection;
   end record;

end Reactive.Infrastructure.Intersection.Intersection_Builder.Mock;
