with Reactive.Infrastructure.Stretch.Decoration.Stretch_Decorator;

package body Reactive.Infrastructure.Stretch.Decoration.Stretch_Sign_Decorator.Utils is

   package Stretch_Decorator_Pkg
      renames Reactive.Infrastructure.Stretch.Decoration.Stretch_Decorator;

   function Get_Instance (
      District_Ref : access Reactive.District.Object'Class := null)
   return Stretch_Sign_Decorator.Utils.Reference is
   begin
      if Instance = null then
         Instance := new Stretch_Sign_Decorator.Utils.Object;
      end if;

      if District_Ref = null then
         Instance.District_Ref := Reactive.District.Get_Instance;
      else
         Instance.District_Ref := District_Ref;
      end if;

      return Instance;
   end Get_Instance;

   not overriding
   function Is_A_Stretch_Sign_Decorator (
      This       : in Stretch_Sign_Decorator.Utils.Object;
      Stretch_Id : in Infra_Id) return Boolean
   is
   begin
      declare
         Test_Variable : access Stretch_Sign_Decorator.Object'Class :=
            Stretch_Sign_Decorator.Reference (
               This.District_Ref.Find_Stretch_By_Id (Stretch_Id));
      begin
         return True;
      end;
   exception
      when Constraint_Error =>
         return False;
   end Is_A_Stretch_Sign_Decorator;

   not overriding
   function Get_Stretch_Ref_Id (
      This       : in Stretch_Sign_Decorator.Utils.Object;
      Stretch_Id : in Infra_Id)
   return Infra_Id
   is
   begin
      return Stretch_Decorator_Pkg.Reference (
               This.District_Ref.Find_Stretch_By_Id (Stretch_Id)
             ).Get_Stretch_Ref_Id;
   end Get_Stretch_Ref_Id;

   not overriding
   function Get_Sign (This       :    Stretch_Sign_Decorator.Utils.Object;
                      Stretch_Id : in Infra_Id)
   return Passive.Road_Sign.Reference
   is
   begin
      return Stretch_Sign_Decorator.Reference (
               This.District_Ref.Find_Stretch_By_Id (Stretch_Id)
             ).Get_Sign;
   end Get_Sign;

end Reactive.Infrastructure.Stretch.Decoration.Stretch_Sign_Decorator.Utils;
