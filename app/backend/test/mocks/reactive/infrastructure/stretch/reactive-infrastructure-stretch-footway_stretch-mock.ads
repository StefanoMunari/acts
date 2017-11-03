with Active.Agent;

package Reactive.Infrastructure.Stretch.Footway_Stretch.Mock is

   package Agent renames Active.Agent;

   type Object is new Footway_Stretch.Object with private;
   type Reference is access all Footway_Stretch.Mock.Object'Class;

   function Create return Footway_Stretch.Mock.Reference;

   overriding
   procedure Tread (This         : in out Footway_Stretch.Mock.Object;
                    Traveller_Id : in     Agent.Agent_Id;
                    Advanced     :    out Boolean);

   overriding
   function Find_Street (This : in Footway_Stretch.Mock.Object)
      return Infra_Id;

   overriding
   function Get_Id (This : in Footway_Stretch.Mock.Object) return Infra_Id;

   overriding
   function Find_Lane (This : in Footway_Stretch.Mock.Object)
                       return Infra_Id;

   overriding
   function Calculate_Position (This : in Footway_Stretch.Mock.Object)
                                return Natural;

   overriding
   function Is_Waiting_To_Enter_Stretch
     (This         : in Footway_Stretch.Mock.Object;
      Traveller_Id : in Agent.Agent_Id) return Boolean;

   overriding
   procedure Leave
     (This         : in out Footway_Stretch.Mock.Object;
      Traveller_Id : in     Agent.Agent_Id;
      Leaved       :    out Boolean);

   overriding
   function Is_Before (This, Other: in Footway_Stretch.Mock.Object) return Boolean;

   procedure Set_Lane (This    : in out Footway_Stretch.Mock.Object;
                       Lane_Id : in     Infra_Id);

   overriding
   function Find_Intersections (This : in Footway_Stretch.Mock.Object)
      return Infra_Id_Set.Set;

   overriding
   function Is_Contained_By (This         : in Footway_Stretch.Mock.Object;
                             Container_Id : in Infra_Id) return Boolean;

   not overriding
   procedure Set_Advanced_Value_For_Tread
     (This     : in out Footway_Stretch.Mock.Object;
      Advanced : in     Boolean);

   not overriding
   procedure Set_Return_Value_For_Find_Street
     (This         : in out Footway_Stretch.Mock.Object;
      Return_Value : in     Infra_Id);

   not overriding
   procedure Set_Return_Value_For_Get_Id
     (This         : in out Footway_Stretch.Mock.Object;
      Return_Value : in     Infra_Id);

   not overriding
   procedure Set_Return_Value_For_Find_Lane
     (This         : in out Footway_Stretch.Mock.Object;
      Return_Value : in     Infra_Id);

   not overriding
   procedure Set_Return_Value_For_Calculate_Position
     (This         : in out Footway_Stretch.Mock.Object;
      Return_Value : in     Natural);

   not overriding
   procedure Set_Return_Value_For_Is_Waiting_To_Enter_Stretch
     (This         : in out Footway_Stretch.Mock.Object;
      Return_Value : in     Boolean);

   not overriding
   procedure Set_Leaved_Value_For_Leave
     (This   : in out Footway_Stretch.Mock.Object;
      Leaved : in     Boolean);

   not overriding
   procedure Set_Return_Value_For_Is_Before
     (This         : in out Footway_Stretch.Mock.Object;
      Return_Value : in     Boolean);

   not overriding
   procedure Set_Return_Value_For_Find_Intersections
     (This         : in out Footway_Stretch.Mock.Object;
      Return_Value : in     Infra_Id_Set.Set);

   not overriding
   procedure Set_Return_Value_For_Is_Contained_By
     (This         : in out Footway_Stretch.Mock.Object;
      Return_Value : in     Boolean);

private
   type Return_Values_Collection is record
      Tread : Boolean;
      Find_Street : Infra_Id;
      Get_Id : Infra_Id;
      Find_Lane : Infra_Id;
      Calculate_Position : Natural;
      Is_Waiting_To_Enter_Stretch : Boolean;
      Leave : Boolean;
      Is_Before : Boolean;
      Find_Intersections : Infra_Id_Set.Set;
      Is_Contained_By : Boolean;
   end record;

   type Object is
     new Footway_Stretch.Object with record
      Return_Values : Return_Values_Collection;
   end record;

   overriding
   procedure Initialize (This : in out Footway_Stretch.Mock.Object) is Null;

   overriding
   procedure Finalize (This : in out Footway_Stretch.Mock.Object) is Null;

end Reactive.Infrastructure.Stretch.Footway_Stretch.Mock;
