package body Reactive.Infrastructure.Stretch.Footway_Stretch.Mock is

   function Create return Footway_Stretch.Mock.Reference
   is (new Footway_Stretch.Mock.Object);

   procedure Set_Lane (This    : in out Footway_Stretch.Mock.Object;
                       Lane_Id : in     Infra_Id) is
   begin
      This.Lane_Id := Lane_Id;
   end Set_Lane;

   procedure Tread (This         : in out Footway_Stretch.Mock.Object;
                    Traveller_Id : in     Agent.Agent_Id;
                    Advanced     :    out Boolean) is
   begin
      Advanced := This.Return_Values.Tread;
   end Tread;

   function Is_Waiting_To_Enter_Stretch
     (This         : in Footway_Stretch.Mock.Object;
      Traveller_Id : in Agent.Agent_Id) return Boolean
   is (This.Return_Values.Is_Waiting_To_Enter_Stretch);

   procedure Leave
     (This         : in out Footway_Stretch.Mock.Object;
      Traveller_Id : in     Agent.Agent_Id;
      Leaved       :    out Boolean) is
   begin
      Leaved := This.Return_Values.Leave;
   end Leave;

   function Get_Id (This : in Footway_Stretch.Mock.Object) return Infra_Id
   is (This.Return_Values.Get_Id);

   function Find_Street (This : in Footway_Stretch.Mock.Object)
                         return Infra_Id
   is (This.Return_Values.Find_Street);

   function Find_Lane (This : in Footway_Stretch.Mock.Object)
                       return Infra_Id
   is (This.Return_Values.Find_Lane);

   function Calculate_Position (This : in Footway_Stretch.Mock.Object)
                                return Natural
   is (This.Return_Values.Calculate_Position);

   function Is_Before (This, Other: in Footway_Stretch.Mock.Object)
                       return Boolean
   is (This.Return_Values.Is_Before);

   function Find_Intersections
     (This : in Footway_Stretch.Mock.Object) return Infra_Id_Set.Set
   is (This.Return_Values.Find_Intersections);

   function Is_Contained_By (This         : in Footway_Stretch.Mock.Object;
                             Container_Id : in Infra_Id) return Boolean
   is (This.Return_Values.Is_Contained_By);

   procedure Set_Advanced_Value_For_Tread
     (This     : in out Footway_Stretch.Mock.Object;
      Advanced : in     Boolean) is
   begin
      This.Return_Values.Tread := Advanced;
   end Set_Advanced_Value_For_Tread;

   procedure Set_Return_Value_For_Find_Street
     (This         : in out Footway_Stretch.Mock.Object;
      Return_Value : in     Infra_Id) is
   begin
      This.Return_Values.Find_Street := Return_Value;
   end Set_Return_Value_For_Find_Street;

   procedure Set_Return_Value_For_Get_Id
     (This         : in out Footway_Stretch.Mock.Object;
      Return_Value : in     Infra_Id) is
   begin
      This.Return_Values.Get_Id := Return_Value;
   end Set_Return_Value_For_Get_Id;

   procedure Set_Return_Value_For_Find_Lane
     (This         : in out Footway_Stretch.Mock.Object;
      Return_Value : in     Infra_Id) is
   begin
      This.Return_Values.Find_Lane := Return_Value;
   end Set_Return_Value_For_Find_Lane;

   procedure Set_Return_Value_For_Calculate_Position
     (This         : in out Footway_Stretch.Mock.Object;
      Return_Value : in     Natural) is
   begin
      This.Return_Values.Calculate_Position := Return_Value;
   end Set_Return_Value_For_Calculate_Position;

   procedure Set_Return_Value_For_Is_Waiting_To_Enter_Stretch
     (This         : in out Footway_Stretch.Mock.Object;
      Return_Value : in     Boolean) is
   begin
      This.Return_Values.Is_Waiting_To_Enter_Stretch := Return_Value;
   end Set_Return_Value_For_Is_Waiting_To_Enter_Stretch;

   procedure Set_Leaved_Value_For_Leave
     (This   : in out Footway_Stretch.Mock.Object;
      Leaved : in     Boolean) is
   begin
      This.Return_Values.Leave := Leaved;
   end Set_Leaved_Value_For_Leave;

   procedure Set_Return_Value_For_Is_Before
     (This         : in out Footway_Stretch.Mock.Object;
      Return_Value : in     Boolean) is
   begin
      This.Return_Values.Is_Before := Return_Value;
   end Set_Return_Value_For_Is_Before;

   procedure Set_Return_Value_For_Find_Intersections
     (This         : in out Footway_Stretch.Mock.Object;
      Return_Value : in     Infra_Id_Set.Set) is
   begin
      This.Return_Values.Find_Intersections := Return_Value;
   end Set_Return_Value_For_Find_Intersections;

   procedure Set_Return_Value_For_Is_Contained_By
     (This         : in out Footway_Stretch.Mock.Object;
      Return_Value : in     Boolean) is
   begin
      This.Return_Values.Is_Contained_By := Return_Value;
   end Set_Return_Value_For_Is_Contained_By;

end Reactive.Infrastructure.Stretch.Footway_Stretch.Mock;
