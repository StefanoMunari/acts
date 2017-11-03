package body Active.Traveller.Vehicle.Mock is

   function Create return Vehicle.Mock.Reference
   is (new Vehicle.Mock.Object);

   function Get_Id (This : in Vehicle.Mock.Object) return Agent.Agent_Id
   is (This.Return_Values.Get_Id);

   function Get_Maximum_Speed (This : in Vehicle.Mock.Object) return Natural
   is (This.Return_Values.Get_Maximum_Speed);

   function Get_Current_Speed (This : in Vehicle.Mock.Object) return Natural
   is (This.Return_Values.Get_Current_Speed);

   procedure Set_Current_Speed (This      : in out Vehicle.Mock.Object;
                                New_Speed : in     Natural) is
   begin
      This.Current_Speed := New_Speed;
   end;

   function Has_Next_Step (This : in Vehicle.Mock.Object) return Boolean
   is (This.Return_Values.Has_Next_Step);

   function Is_Travelling (This : in Vehicle.Mock.Object) return Boolean
   is (This.Return_Values.Is_Travelling);

   function "=" (This, Other : Vehicle.Mock.Object) return Boolean
   is (This.Return_Values.Equality_Operator);

   function Get_List_From_Slice (This : in Vehicle.Mock.Object;
                                 The_Slice    : in     Slice.Map)
   return Infra_Id_List.List is (This.Return_Values.The_List);

   procedure Board (This    : in out Vehicle.Mock.Object;
                    Incomer : in     Agent.Agent_Id;
                    Boarded :    out Boolean) is
   begin
      Boarded := This.Return_Values.Board;
      This.Return_Values.Board_Called := True;
   end Board;

   procedure Free (This      : in out Vehicle.Mock.Object;
                   Passenger : in     Agent.Agent_Id;
                   Freed     :    out Boolean) is
   begin
      Freed := This.Return_Values.Free;
      This.Return_Values.Free_Called := True;
   end Free;

   function Get_Passengers   (This : in Vehicle.Mock.Object)
   return Agent_Id_List.List is (This.Return_Values.Get_Passengers);

   function Count_Passengers (This : in Vehicle.Mock.Object) return Natural
   is (This.Return_Values.Count_Passengers);

   function Is_Affected_By_Traffic_Lights (This : in Vehicle.Mock.Object)
   return Boolean is (This.Return_Values.Affected_By_Traffic_Lights);

   function Get_Size (This : in Mock.Object)
   return Natural is (This.Return_Values.Get_Size);

   procedure Set_Return_Value_For_Get_Id (
      This  : in out Vehicle.Mock.Object;
      Return_Value : in Agent.Agent_Id) is
   begin
      This.Return_Values.Get_Id := Return_Value;
   end Set_Return_Value_For_Get_Id;

   procedure Set_Return_Value_For_Get_Maximum_Speed (
      This  : in out Vehicle.Mock.Object;
      Return_Value : in Natural) is
   begin
      This.Return_Values.Get_Maximum_Speed := Return_Value;
   end Set_Return_Value_For_Get_Maximum_Speed;

   procedure Set_Return_Value_For_Get_Current_Speed (
      This  : in out Vehicle.Mock.Object;
      Return_Value : in Natural) is
   begin
      This.Return_Values.Get_Current_Speed := Return_Value;
   end Set_Return_Value_For_Get_Current_Speed;

   procedure Set_Return_Value_For_Has_Next_Step (
      This  : in out Vehicle.Mock.Object;
      Return_Value : in Boolean) is
   begin
      This.Return_Values.Has_Next_Step := Return_Value;
   end Set_Return_Value_For_Has_Next_Step;

   procedure Set_Return_Value_For_Is_Travelling (
      This  : in out Vehicle.Mock.Object;
      Return_Value : in Boolean) is
   begin
      This.Return_Values.Is_Travelling := Return_Value;
   end Set_Return_Value_For_Is_Travelling;

   procedure Set_Return_Value_For_Equality_Operator (
      This  : in out Vehicle.Mock.Object;
      Return_Value : in Boolean) is
   begin
      This.Return_Values.Equality_Operator := Return_Value;
   end Set_Return_Value_For_Equality_Operator;

   procedure Set_List_Value_For_Get_List_From_Slice (
      This     : in out Vehicle.Mock.Object;
      The_List : in     Infra_Id_List.List) is
   begin
      This. Return_Values.The_List := The_List;
   end Set_List_Value_For_Get_List_From_Slice;

   procedure Set_Return_Value_For_Board (
      This  : in out Vehicle.Mock.Object;
      Boarded : in Boolean) is
   begin
      This.Return_Values.Board := Boarded;
   end Set_Return_Value_For_Board;

   procedure Set_Return_Value_For_Free (
      This  : in out Vehicle.Mock.Object;
      Freed : in Boolean) is
   begin
      This.Return_Values.Free := Freed;
   end Set_Return_Value_For_Free;

   procedure Set_Return_Value_For_Get_Passengers (
      This         : in out Vehicle.Mock.Object;
      Return_Value : in     Agent_Id_List.List) is
   begin
      This.Return_Values.Get_Passengers := Return_Value;
   end;

   procedure Set_Return_Value_For_Count_Passengers (
      This  : in out Vehicle.Mock.Object;
      Return_Value : in Natural) is
   begin
      This.Return_Values.Count_Passengers := Return_Value;
   end Set_Return_Value_For_Count_Passengers;

   not overriding
   procedure Set_Return_Value_For_Is_Affected_By_Traffic_Lights (
      This         : in out Vehicle.Mock.Object;
      Return_Value : in     Boolean) is
   begin
      This.Return_Values.Affected_By_Traffic_Lights := Return_Value;
   end Set_Return_Value_For_Is_Affected_By_Traffic_Lights;

   procedure Set_Return_Value_For_Get_Size (
      This         : in out Vehicle.Mock.Object;
      Return_Value : in     Natural) is
   begin
      This.Return_Values.Get_Size := Return_Value;
   end Set_Return_Value_For_Get_Size;

   function Get_Free_Called (This : in out Mock.Object) return Boolean
   is (This.Return_Values.Free_Called);

   function Get_Board_Called (This : in out Mock.Object) return Boolean
   is (This.Return_Values.Board_Called);

end Active.Traveller.Vehicle.Mock;
