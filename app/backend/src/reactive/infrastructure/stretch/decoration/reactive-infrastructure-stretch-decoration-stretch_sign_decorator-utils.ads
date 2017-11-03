with Reactive.District;

package Reactive.Infrastructure.Stretch.Decoration.Stretch_Sign_Decorator.Utils is

   type Object (<>) is tagged limited private;
   type Reference is access all Stretch_Sign_Decorator.Utils.Object'Class;

   function Get_Instance (
      District_Ref : access Reactive.District.Object'Class := null)
   return Stretch_Sign_Decorator.Utils.Reference;

   not overriding
   function Is_A_Stretch_Sign_Decorator (
      This       : in Stretch_Sign_Decorator.Utils.Object;
      Stretch_Id : in Infra_Id) return Boolean;

   not overriding
   function Get_Stretch_Ref_Id (
      This       : in Stretch_Sign_Decorator.Utils.Object;
      Stretch_Id : in Infra_Id)
   return Infra_Id;

   not overriding
   function Get_Sign (This       :    Stretch_Sign_Decorator.Utils.Object;
                      Stretch_Id : in Infra_Id)
   return Passive.Road_Sign.Reference;

private

   type Object is tagged limited record
      District_Ref : access Reactive.District.Object'Class;
   end record;

   Instance : Stretch_Sign_Decorator.Utils.Reference := null;

end Reactive.Infrastructure.Stretch.Decoration.Stretch_Sign_Decorator.Utils;
