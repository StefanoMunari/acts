with Shared.Infra_Id_To_Boolean_Map;

package Reactive.Infrastructure.Stretch.Decoration.Stretch_Sign_Decorator.Utils.Mock is

   package Infra_Id_To_Boolean_Map renames Shared.Infra_Id_To_Boolean_Map;

   type Object is new Stretch_Sign_Decorator.Utils.Object with private;
   type Reference is access all Utils.Mock.Object'Class;

   function Create
   return Utils.Mock.Reference;

   overriding
   function Is_A_Stretch_Sign_Decorator (
      This       : in Utils.Mock.Object;
      Stretch_Id : in Infra_Id)
   return Boolean;

   overriding
   function Get_Sign (This       :    Utils.Mock.Object;
                      Stretch_Id : in Infra_Id)
   return Passive.Road_Sign.Reference;

   not overriding
   procedure Set_Return_Value_For_Is_A_Stretch_Sign_Decorator (
      This         : in out Utils.Mock.Object;
      Stretch_Id   : in     Infra_Id;
      Return_Value : in     Boolean);

   not overriding
   procedure Set_Return_Value_For_Get_Sign (
      This         : in out Utils.Mock.Object;
      Return_Value : in     Passive.Road_Sign.Reference);

private
   type Return_Values_Collection is record
      Is_A_Stretch_Sign_Decorator : Infra_Id_To_Boolean_Map.Map;
      Get_Sign           : Passive.Road_Sign.Reference;
      Get_Sign_Existence : Boolean;
   end record;

   type Object is new Stretch_Sign_Decorator.Utils.Object with record
      Return_Values : Return_Values_Collection;
   end record;

end Reactive.Infrastructure.Stretch.Decoration.Stretch_Sign_Decorator.Utils.Mock;
