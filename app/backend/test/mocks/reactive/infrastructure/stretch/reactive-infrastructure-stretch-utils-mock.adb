with Mock.Exceptions;

use Mock.Exceptions;

package body Reactive.Infrastructure.Stretch.Utils.Mock is

   function Create return Stretch.Utils.Mock.Reference
   is (new Stretch.Utils.Mock.Object);

   function Get_Id (This : in Stretch.Utils.Mock.Object;
                    Infrastructure_Id : in Infra_Id) return Infra_Id is
   begin
      if not This.Return_Values.Get_Id.Contains (Infrastructure_Id) then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception
           (Function_Name => "Get_Id",
            Function_Param => "Infrastructure_Id = "
            & Infra_Id'Image(Infrastructure_Id),
            Package_Name  => "Stretch.Utils.Mock");
      end if;

      return This.Return_Values.Get_Id.Element (Key => Infrastructure_Id);
   end Get_Id;

   procedure Tread (
      This         : in     Stretch.Utils.Mock.Object;
      Stretch_Id   : in     Infra_Id;
      Traveller_Id : in     Agent.Agent_Id;
      Advanced     :    out Boolean) is
   begin
      if not This.Return_Values.Tread_Existence then
         Raise_Missing_Return_Value_For_Mocked_Procedure_Exception
           (Parameter_Name => "Advanced",
            Procedure_Name => "Tread",
            Package_Name => "Stretch.Utils.Mock");
      end if;

      Advanced := This.Return_Values.Tread;
   end Tread;

   function Is_Waiting_To_Enter_Stretch (
      This         : in Stretch.Utils.Mock.Object;
      Stretch_Id   : in Infra_Id;
      Traveller_Id : in Agent.Agent_Id) return Boolean is
   begin
      if not This.Return_Values.Is_Waiting_To_Enter_Stretch_Existence then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception
           (Function_Name => "Is_Waiting_To_Enter_Stretch",
            Package_Name  => "Stretch.Utils.Mock");
      end if;

      return This.Return_Values.Is_Waiting_To_Enter_Stretch;
   end Is_Waiting_To_Enter_Stretch;

   procedure Leave (
      This         : in     Stretch.Utils.Mock.Object;
      Stretch_Id   : in     Infra_Id;
      Traveller_Id : in     Agent.Agent_Id;
      Left         :    out Boolean) is
   begin
      if not This.Return_Values.Leave_Existence then
         Raise_Missing_Return_Value_For_Mocked_Procedure_Exception
           (Parameter_Name => "Left",
            Procedure_Name => "Leave",
            Package_Name => "Stretch.Utils.Mock");
      end if;

      Left := This.Return_Values.Leave;
   end Leave;

   function Is_Before (This : in Stretch.Utils.Mock.Object;
                       Left, Right: in Infra_Id) return Boolean is
   begin
      if not This.Return_Values.Is_Before_Existence.Contains (Left)
        or not This.Return_Values.Is_Before_Existence.Element (Left).Contains (Right)
      then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception
           (Function_Name => "Is_Before",
            Package_Name  => "Stretch.Utils.Mock");
      end if;

      return This.Return_Values.Is_Before.Contains (Left)
        and then This.Return_Values.Is_Before.Element (Left).Contains (Right);
   end Is_Before;

   function Get_Host (This       : in Stretch.Utils.Mock.Object;
                      Stretch_Id : in Infra_Id)
   return Infra_Id is
   begin
      if not This.Return_Values.Get_Host.Contains (Stretch_Id) then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception
           (Function_Name => "Get_Host",
            Function_Param => "Stretch_Id = "
            & Infra_Id'Image(Stretch_Id),
            Package_Name  => "Stretch.Utils.Mock");
      end if;

      return This.Return_Values.Get_Host.Element (Key => Stretch_Id);
   end Get_Host;

   function Has_Host (This       : in Stretch.Utils.Mock.Object;
                      Stretch_Id : in Infra_Id)
   return Boolean is
   begin
      if not This.Return_Values.Has_Host.Contains (Stretch_Id) then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception
           (Function_Name => "Has_Host",
            Function_Param => "Stretch_Id = "
            & Infra_Id'Image(Stretch_Id),
            Package_Name  => "Stretch.Utils.Mock");
      end if;

      return This.Return_Values.Has_Host.Element (Key => Stretch_Id);
   end Has_Host;

   function Find_Intersections (This : in Stretch.Utils.Mock.Object;
                                Stretch_Id : in Infra_Id)
                                return Infra_Id_Set.Set is
   begin
      if not This.Return_Values.Find_Intersections.Contains (Stretch_Id) then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception
           (Function_Name => "Find_Intersections",
            Package_Name  => "Stretch.Utils.Mock");
      end if;

      return This.Return_Values.Find_Intersections.Element (Stretch_Id);
   end Find_Intersections;

   procedure Set_Return_Value_For_Get_Id (
      This              : in out Stretch.Utils.Mock.Object;
      Infrastructure_Id : in Infra_Id;
      Stretch_Id        : in Infra_Id) is
   begin
      This.Return_Values.Get_Id.Insert (Key => Infrastructure_Id,
                                        New_Item => Stretch_Id);
   end Set_Return_Value_For_Get_Id;

   procedure Set_Return_Value_For_Tread (
      This         : in out Stretch.Utils.Mock.Object;
      Return_Value : in Boolean) is
   begin
      This.Return_Values.Tread := Return_Value;
      This.Return_Values.Tread_Existence := TRUE;
   end Set_Return_Value_For_Tread;

   procedure Set_Return_Value_For_Is_Waiting_To_Enter_Stretch (
      This         : in out Stretch.Utils.Mock.Object;
      Return_Value : in Boolean) is
   begin
      This.Return_Values.Is_Waiting_To_Enter_Stretch := Return_Value;
      This.Return_Values.Is_Waiting_To_Enter_Stretch_Existence := TRUE;
   end Set_Return_Value_For_Is_Waiting_To_Enter_Stretch;

   procedure Set_Return_Value_For_Leave (
      This         : in out Stretch.Utils.Mock.Object;
      Return_Value : in Boolean) is
   begin
      This.Return_Values.Leave := Return_Value;
      This.Return_Values.Leave_Existence := TRUE;
   end Set_Return_Value_For_Leave;

   procedure Set_Return_Value_For_Is_Before (
      This         : in out Stretch.Utils.Mock.Object;
      Stretch1_Id  : in Infra_Id;
      Stretch2_Id  : in Infra_Id;
      Return_Value : in Boolean)
   is
      Is_Before_Stretch1 : Infra_Id_Set.Set;
   begin
      if Return_Value then
         if This.Return_Values.Is_Before.Contains (Stretch1_Id) then
            Is_Before_Stretch1 := This.Return_Values.Is_Before
              .Element (Key => Stretch1_Id);

            Is_Before_Stretch1.Insert (Stretch2_Id);

            This.Return_Values.Is_Before
              .Replace (Key      => Stretch1_Id,
                        New_Item => Is_Before_Stretch1);
         else
            Is_Before_Stretch1.Insert (Stretch2_Id);
            This.Return_Values.Is_Before
              .Insert (Key      => Stretch1_Id,
                       New_Item => Is_Before_Stretch1);
         end if;

         Is_Before_Stretch1 := Infra_Id_Set.Empty_Set;
      end if;

      if This.Return_Values.Is_Before_Existence.Contains (Stretch1_Id) then
         Is_Before_Stretch1 := This.Return_Values.Is_Before_Existence
           .Element(Key => Stretch1_Id);

         Is_Before_Stretch1.Insert (Stretch2_Id);

         This.Return_Values.Is_Before_Existence
           .Replace (Key      => Stretch1_Id,
                     New_Item => Is_Before_Stretch1);
      else
         Is_Before_Stretch1.Insert (Stretch2_Id);

         This.Return_Values.Is_Before_Existence
           .Insert(Key      => Stretch1_Id,
                   New_Item => Is_Before_Stretch1);
      end if;
   end Set_Return_Value_For_Is_Before;

   procedure Set_Return_Value_For_Find_Intersections (
      This         : in out Stretch.Utils.Mock.Object;
      Stretch_Id   : in Infra_Id;
      Return_Value : in Infra_Id_Set.Set) is
   begin
      This.Return_Values.Find_Intersections.Insert (Key      => Stretch_Id,
                                                    New_Item => Return_Value);
   end Set_Return_Value_For_Find_Intersections;

   procedure Set_Return_Value_For_Get_Host (
      This         : in out Stretch.Utils.Mock.Object;
      Stretch_Id   : in Infra_Id;
      Return_Value : in Infra_Id) is
   begin
      This.Return_Values.Get_Host.Insert (Key      => Stretch_Id,
                                          New_Item => Return_Value);
   end Set_Return_Value_For_Get_Host;

   procedure Set_Return_Value_For_Has_Host (
      This         : in out Stretch.Utils.Mock.Object;
      Stretch_Id   : in Infra_Id;
      Return_Value : in Boolean) is
   begin
      This.Return_Values.Has_Host.Insert (Key      => Stretch_Id,
                                          New_Item => Return_Value);
   end Set_Return_Value_For_Has_Host;

end Reactive.Infrastructure.Stretch.Utils.Mock;
