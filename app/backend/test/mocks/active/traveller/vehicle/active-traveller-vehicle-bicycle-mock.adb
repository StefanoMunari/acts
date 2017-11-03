package body Active.Traveller.Vehicle.Bicycle.Mock is

   function Create return Bicycle.Mock.Reference
   is (new Bicycle.Mock.Object);

   function Get_Id (This : in Bicycle.Mock.Object) return Agent.Agent_Id
   is (This.Return_Values.Get_Id);

   function Get_Maximum_Speed (This : in Bicycle.Mock.Object) return Natural
   is (This.Return_Values.Get_Maximum_Speed);

   function Get_Current_Speed (This : in Bicycle.Mock.Object) return Natural
   is (This.Return_Values.Get_Current_Speed);

   procedure Set_Current_Speed (This      : in out Bicycle.Mock.Object;
                                New_Speed : in     Natural) is
   begin
      This.Current_Speed := New_Speed;
   end;

   function Has_Next_Step (This : in Bicycle.Mock.Object) return Boolean
   is (This.Return_Values.Has_Next_Step);

   function Is_Travelling (This : in Bicycle.Mock.Object) return Boolean
   is (This.Return_Values.Is_Travelling);

   function "=" (This, Other : Bicycle.Mock.Object) return Boolean
   is (This.Return_Values.Equality_Operator);

   procedure Board (This    : in out Bicycle.Mock.Object;
                    Incomer : in     Agent.Agent_Id;
                    Boarded :    out Boolean) is
   begin
      Boarded := This.Return_Values.Board;
   end Board;

   procedure Free (This      : in out Bicycle.Mock.Object;
                   Passenger : in     Agent.Agent_Id;
                   Freed     :    out Boolean) is
   begin
      Freed := This.Return_Values.Free;
   end Free;

   function Count_Passengers (This : in Bicycle.Mock.Object) return Natural
   is (This.Return_Values.Count_Passengers);

   procedure Set_Return_Value_For_Get_Id (
      This  : in out Bicycle.Mock.Object;
      Return_Value : in Agent.Agent_Id) is
   begin
      This.Return_Values.Get_Id := Return_Value;
   end Set_Return_Value_For_Get_Id;

   procedure Set_Return_Value_For_Get_Maximum_Speed (
      This  : in out Bicycle.Mock.Object;
      Return_Value : in Natural) is
   begin
      This.Return_Values.Get_Maximum_Speed := Return_Value;
   end Set_Return_Value_For_Get_Maximum_Speed;

   procedure Set_Return_Value_For_Get_Current_Speed (
      This  : in out Bicycle.Mock.Object;
      Return_Value : in Natural) is
   begin
      This.Return_Values.Get_Current_Speed := Return_Value;
   end Set_Return_Value_For_Get_Current_Speed;

   procedure Set_Return_Value_For_Has_Next_Step (
      This  : in out Bicycle.Mock.Object;
      Return_Value : in Boolean) is
   begin
      This.Return_Values.Has_Next_Step := Return_Value;
   end Set_Return_Value_For_Has_Next_Step;

   procedure Set_Return_Value_For_Is_Travelling (
      This  : in out Bicycle.Mock.Object;
      Return_Value : in Boolean) is
   begin
      This.Return_Values.Is_Travelling := Return_Value;
   end Set_Return_Value_For_Is_Travelling;

   procedure Set_Return_Value_For_Equality_Operator (
      This  : in out Bicycle.Mock.Object;
      Return_Value : in Boolean) is
   begin
      This.Return_Values.Equality_Operator := Return_Value;
   end Set_Return_Value_For_Equality_Operator;

   procedure Set_Boarded_Value_For_Board (
      This  : in out Bicycle.Mock.Object;
      Boarded : in Boolean) is
   begin
      This.Return_Values.Board := Boarded;
   end Set_Boarded_Value_For_Board;

   procedure Set_Freed_Value_For_Free (
      This  : in out Bicycle.Mock.Object;
      Freed : in Boolean) is
   begin
      This.Return_Values.Free := Freed;
   end Set_Freed_Value_For_Free;

   procedure Set_Return_Value_For_Count_Passengers (
      This  : in out Bicycle.Mock.Object;
      Return_Value : in Natural) is
   begin
      This.Return_Values.Count_Passengers := Return_Value;
   end Set_Return_Value_For_Count_Passengers;

end Active.Traveller.Vehicle.Bicycle.Mock;
