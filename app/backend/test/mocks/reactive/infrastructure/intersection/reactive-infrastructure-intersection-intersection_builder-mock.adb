with Mock.Exceptions;

use Mock.Exceptions;

package body Reactive.Infrastructure.Intersection.Intersection_Builder.Mock is

   use Agent;

   function Create return Intersection_Builder.Mock.Reference
   is (new Intersection_Builder.Mock.Object);

   procedure With_Street (This      : in out Intersection_Builder.Mock.Object;
                          Street_Id  : in     Infra_Id;
                          Stretches : in     Infra_Id_List.List;
                          Direction  : in     Shared.Direction.Cardinal) is
   begin
      This.Mock_Values.With_Street_Called.Insert (Street_Id, Direction);
   end With_Street;

   procedure With_Traffic_Light (
      This           : in out Intersection_Builder.Mock.Object;
      Traffic_Light  : in     Agent.Agent_Id;
      Exit_Direction : in     Direction.Cardinal) is
   begin
      This.Mock_Values.With_Traffic_Light_Called.Insert (
         Exit_Direction, Traffic_Light);
   end With_Traffic_Light;

   function Get_Result (This : in Intersection_Builder.Mock.Object)
   return Infra_Id is
   begin
      if not This.Return_Values.Get_Result_Existence then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception (
            Function_Name  => "Get_Result",
            Function_Param => "Return value",
            Package_Name   =>
              "Reactive.Infrastructure.Intersection.Intersection_Builder.Mock");
      end if;

      return This.Return_Values.Get_Result;
   end Get_Result;

   procedure Set_Return_Value_For_Get_Result (
      This         : in out Intersection_Builder.Mock.Object;
      Return_Value : in     Infra_Id) is
   begin
      This.Return_Values.Get_Result := Return_Value;
      This.Return_Values.Get_Result_Existence := True;
   end Set_Return_Value_For_Get_Result;

   function Get_With_Street_Called (
      This      : in out Intersection_Builder.Mock.Object;
      Street_Id : in     Infra_Id;
      Direction :        Shared.Direction.Cardinal) return Boolean
   is
   begin
      if not This.Mock_Values.With_Street_Called.Contains (Street_Id) then
         return False;
      end if;
      return Direction =
             This.Mock_Values.With_Street_Called.Element (Street_Id);
   end Get_With_Street_Called;

   function Get_With_Traffic_Light_Called (
      This           : in out Intersection_Builder.Mock.Object;
      Traffic_Light  : in     Agent.Agent_Id;
      Exit_Direction : in     Shared.Direction.Cardinal) return Boolean
   is
   begin
      if not This.Mock_Values.With_Traffic_Light_Called.Contains (
               Exit_Direction)
      then
         return False;
      end if;
      return Traffic_Light =
             This.Mock_Values.With_Traffic_Light_Called.Element (
               Exit_Direction);
   end Get_With_Traffic_Light_Called;

end Reactive.Infrastructure.Intersection.Intersection_Builder.Mock;
