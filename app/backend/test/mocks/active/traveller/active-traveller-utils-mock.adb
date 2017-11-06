with Reactive.District.Mock;
with Mock.Exceptions;

use Mock.Exceptions;

package body Active.Traveller.Utils.Mock is

   function Create return Traveller.Utils.Mock.Reference
   is (new Traveller.Utils.Mock.Object);

   function Get_Stretch_Type (
      This         : in Traveller.Utils.Mock.Object;
      Traveller_Id : in Agent.Agent_Id)
   return Stretch_Type is
   begin
      if not This.Return_Values.Get_Stretch_Type_Existence then
         Raise_Missing_Return_Value_For_Mocked_Procedure_Exception (
            Procedure_Name => "Get_Stretch_Type",
            Package_Name   => "Active.Traveller.Utils.Mock");
      end if;

      return This.Return_Values.Get_Stretch_Type;
   end Get_Stretch_Type;

   procedure Consume_Step (
      This         : in out Traveller.Utils.Mock.Object;
      Traveller_Id : in     Agent.Agent_Id) is
   begin
      This.Mock_Values.Consume_Step_Called := True;
   end Consume_Step;

   function Get_Position (This         : in     Traveller.Utils.Mock.Object;
                          Traveller_Id : in     Agent.Agent_Id)
   return Infra_Id is
   begin
      if not This.Return_Values.Get_Position_Existence then
         Raise_Missing_Return_Value_For_Mocked_Procedure_Exception (
            Procedure_Name => "Get_Position",
            Package_Name   => "Active.Traveller.Utils.Mock");
      end if;

      return This.Return_Values.Get_Position;
   end Get_Position;

   -- TOOD here
   function Get_Next_Step (This         : in     Traveller.Utils.Mock.Object;
                          Traveller_Id : in     Agent.Agent_Id)
   return Infra_Id is
   begin
      if not This.Return_Values.Get_Next_Step.Contains (Traveller_Id) then
         Raise_Missing_Return_Value_For_Mocked_Procedure_Exception (
            Procedure_Name => "Get_Next_Step",
            Package_Name   => "Active.Traveller.Utils.Mock");
      end if;

      return This.Return_Values.Get_Next_Step.Element (Traveller_Id);
   end Get_Next_Step;

   procedure Set_Current_Speed (This      : in     Traveller.Utils.Mock.Object;
                                Traveller_Id : in     Agent.Agent_Id;
                                New_Speed : in     Natural) is
   begin
      if not This.Return_Values.Traveller_Ref_Speed_Existence then
         Raise_Missing_Return_Value_For_Mocked_Procedure_Exception (
            Procedure_Name => "Get_Current_Speed",
            Package_Name   => "Active.Traveller.Utils.Mock");
      end if;

      This.Return_Values.Traveller_Ref_Speed.Set_Current_Speed (New_Speed);
   end Set_Current_Speed;

   function Get_Current_Speed (This      : in     Traveller.Utils.Mock.Object;
                               Traveller_Id : in     Agent.Agent_Id)
   return Natural
   is
   begin
      if not This.Return_Values.Traveller_Ref_Speed_Existence then
         Raise_Missing_Return_Value_For_Mocked_Procedure_Exception (
            Procedure_Name => "Get_Current_Speed",
            Package_Name   => "Active.Traveller.Utils.Mock");
      end if;

      return This.Return_Values.Traveller_Ref_Speed.Get_Current_Speed;
   end Get_Current_Speed;

   function Get_Travel_Source (This         : in     Mock.Object;
                                    Traveller_Id : in     Agent.Agent_Id)
      return Slice.Map is
    begin
      if not This.Return_Values.Get_Travel_Source.Contains (Traveller_Id)
      then
         Raise_Missing_Return_Value_For_Mocked_Procedure_Exception (
            Procedure_Name => "Get_Travel_Source",
            Package_Name   => "Active.Traveller.Utils.Mock");
      end if;

      return This.Return_Values.Get_Travel_Source.Element (Traveller_Id);
    end Get_Travel_Source;

   function Get_Travel_Destination (This         : in     Mock.Object;
                                    Traveller_Id : in     Agent.Agent_Id)
      return Slice.Map is
    begin
      if not This.Return_Values.Get_Travel_Destination.Contains (Traveller_Id)
      then
         Raise_Missing_Return_Value_For_Mocked_Procedure_Exception (
            Procedure_Name => "Get_Travel_Destination",
            Package_Name   => "Active.Traveller.Utils.Mock");
      end if;

      return This.Return_Values.Get_Travel_Destination.Element (Traveller_Id);
    end Get_Travel_Destination;

   function Does_Travel_Contain_Step (
      This         : in Traveller.Utils.Mock.Object;
      Traveller_Id : in     Agent.Agent_Id;
      Step         : in Infra_Id)
   return Boolean is
   begin
      if not This.Return_Values.Does_Travel_Contain_Step.Contains (Step) then
         Raise_Missing_Return_Value_For_Mocked_Procedure_Exception (
            Procedure_Name => "Does_Travel_Contain_Step",
            Package_Name   => "Active.Traveller.Utils.Mock");
      end if;

      return This.Return_Values.Does_Travel_Contain_Step.Element (Step);
   end Does_Travel_Contain_Step;

   function Does_Travel_Contain_Steps (
      This         : in     Traveller.Utils.Mock.Object;
      Traveller_Id : in     Agent.Agent_Id;
      Step         : in     Slice.Map)
   return Boolean is
   begin
      return
         This.Return_Values.Does_Travel_Contain_Steps.Contains (Traveller_Id);
   end Does_Travel_Contain_Steps;

   function Get_List_From_Slice (This         : in     Mock.Object;
                                 Traveller_Id : in     Agent.Agent_Id;
                                 Slice_Obj    : in     Slice.Map)
      return Infra_Id_List.List is
    begin
      if not This.Return_Values.Get_List_From_Slice_Existence then
         Raise_Missing_Return_Value_For_Mocked_Procedure_Exception (
            Procedure_Name => "Get_List_From_Slice",
            Package_Name   => "Active.Traveller.Utils.Mock");
      end if;

      return This.Return_Values.Get_List_From_Slice;
    end Get_List_From_Slice;

   procedure Defer (This         : in out Traveller.Utils.Mock.Object;
                    Traveller_Id : in     Agent.Agent_Id;
                    Retry_Action : in     Boolean)
   is
   begin
   -- TODO: Add implementation
      null;
   end;

   function Get_Size (
      This         : in Traveller.Utils.Mock.Object;
      Traveller_Id : in Agent.Agent_Id) return Natural is
   begin
      if not This.Return_Values.Get_Size_Existence then
         Raise_Missing_Return_Value_For_Mocked_Procedure_Exception (
            Procedure_Name => "Get_Size",
            Package_Name   => "Active.Traveller.Utils.Mock");
      end if;

      return This.Return_Values.Get_Size;
   end Get_Size;

   procedure Set_Return_Value_For_Get_Stretch_Type (
      This         : in out Traveller.Utils.Mock.Object;
      Return_Value : in     Stretch_Type) is
   begin
      This.Return_Values.Get_Stretch_Type := Return_Value;
      This.Return_Values.Get_Stretch_Type_Existence := True;
   end Set_Return_Value_For_Get_Stretch_Type;

   procedure Set_Return_Value_For_Get_Next_Step (
      This         : in out Traveller.Utils.Mock.Object;
      Traveller_Id : in     Agent.Agent_Id;
      Return_Value : in     Infra_Id) is
   begin
      This.Return_Values.Get_Next_Step.Insert (Traveller_Id, Return_Value);
   end Set_Return_Value_For_Get_Next_Step;

   procedure Set_Return_Value_For_Get_Position (
      This         : in out Traveller.Utils.Mock.Object;
      Return_Value : in     Infra_Id) is
   begin
      This.Return_Values.Get_Position := Return_Value;
      This.Return_Values.Get_Position_Existence := TRUE;
   end Set_Return_Value_For_Get_Position;

   procedure Set_Traveller_For_Set_Current_Speed (
      This          : in out Traveller.Utils.Mock.Object;
      Traveller_Ref : in Active.Traveller.Mock.Reference) is
     begin
      This.Return_Values.Traveller_Ref_Speed := Traveller_Ref;
      This.Return_Values.Traveller_Ref_Speed_Existence := TRUE;
     end Set_Traveller_For_Set_Current_Speed;

   procedure Set_Return_Value_For_Get_Travel_Source (
      This         : in out Traveller.Utils.Mock.Object;
      Traveller_Id : in     Agent.Agent_Id;
      Source       : in     Slice.Map)
   is
   begin
      This.Return_Values.Get_Travel_Source.Include (
         Key      => Traveller_Id,
         New_Item => Source);
   end Set_Return_Value_For_Get_Travel_Source;

   procedure Set_Return_Value_For_Get_Travel_Destination (
      This         : in out Traveller.Utils.Mock.Object;
      Traveller_Id : in     Agent.Agent_Id;
      Destination  : in     Slice.Map)
   is
   begin
      This.Return_Values.Get_Travel_Destination.Include (
         Key      => Traveller_Id,
         New_Item => Destination);
   end Set_Return_Value_For_Get_Travel_Destination;

   procedure Set_Return_Value_For_Does_Travel_Contain_Step (
      This         : in out Traveller.Utils.Mock.Object;
      Stretch_Id     : in     Infra_Id;
      Return_Value : in     Boolean) is
   begin
      This.Return_Values.Does_Travel_Contain_Step.Insert (
         Stretch_Id, Return_Value);
   end Set_Return_Value_For_Does_Travel_Contain_Step;

   procedure Set_Return_Value_For_Does_Travel_Contain_Steps (
      This         : in out Traveller.Utils.Mock.Object;
      Traveller_Id : in     Agent.Agent_Id) is
   begin
      This.Return_Values.Does_Travel_Contain_Steps.Append (Traveller_Id);
      This.Return_Values.Does_Travel_Contain_Steps_Existence := True;
   end Set_Return_Value_For_Does_Travel_Contain_Steps;

   procedure Set_Return_Value_For_Get_List_From_Slice (
      This     : in out Traveller.Utils.Mock.Object;
      The_List : in     Infra_Id_List.List) is
   begin
      This.Return_Values.Get_List_From_Slice := The_List;
      This.Return_Values.Get_List_From_Slice_Existence := TRUE;
   end Set_Return_Value_For_Get_List_From_Slice;

   procedure Set_Return_Value_For_Get_Size (
      This     : in out Traveller.Utils.Mock.Object;
      Size     : in     Natural) is
   begin
      This.Return_Values.Get_Size := Size;
      This.Return_Values.Get_Size_Existence := TRUE;
   end Set_Return_Value_For_Get_Size;

   function Get_Consume_Step_Called (
      This : in out Traveller.Utils.Mock.Object)
   return Boolean is (This.Mock_Values.Consume_Step_Called);

end Active.Traveller.Utils.Mock;
