with Mock.Exceptions;

use Mock.Exceptions;

package body Reactive.Infrastructure.Stretch.Decoration.Stretch_Sign_Decorator.Utils.Mock is

   function Create
   return Utils.Mock.Reference
   is (new Utils.Mock.Object);

   function Is_A_Stretch_Sign_Decorator (
      This       : in Utils.Mock.Object;
      Stretch_Id : in Infra_Id)
   return Boolean
   is
   begin
      if not This.Return_Values.Is_A_Stretch_Sign_Decorator.Contains (
               Stretch_Id)
      then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception (
            Function_Name => "Is_A_Stretch_Sign_Decorator",
            Function_Param => "Stretch_Id = "
                              & Infra_Id'Image(Stretch_Id),
            Package_Name  => "Stretch_Sign_Decorator.Utils.Mock");
      end if;

      return
         This.Return_Values.Is_A_Stretch_Sign_Decorator.Element (
            Key => Stretch_Id);
   end Is_A_Stretch_Sign_Decorator;

   function Get_Sign (This       :    Utils.Mock.Object;
                      Stretch_Id : in Infra_Id)
   return Passive.Road_Sign.Reference
   is
   begin
      if not This.Return_Values.Get_Sign_Existence
      then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception (
            Function_Name => "Get_Sign",
            Package_Name  => "Stretch_Sign_Decorator.Utils.Mock");
      end if;

      return This.Return_Values.Get_Sign;
   end Get_Sign;

   procedure Set_Return_Value_For_Is_A_Stretch_Sign_Decorator (
      This         : in out Utils.Mock.Object;
      Stretch_Id   : in     Infra_Id;
      Return_Value : in     Boolean) is
   begin
      This.Return_Values.Is_A_Stretch_Sign_Decorator.Insert (
         Key      => Stretch_Id,
         New_Item => Return_Value);
   end Set_Return_Value_For_Is_A_Stretch_Sign_Decorator;

   procedure Set_Return_Value_For_Get_Sign (
      This         : in out Utils.Mock.Object;
      Return_Value : in     Passive.Road_Sign.Reference)
   is
   begin
      This.Return_Values.Get_Sign := Return_Value;
      This.Return_Values.Get_Sign_Existence := True;
   end Set_Return_Value_For_Get_Sign;

end Reactive.Infrastructure.Stretch.Decoration.Stretch_Sign_Decorator.Utils.Mock;
