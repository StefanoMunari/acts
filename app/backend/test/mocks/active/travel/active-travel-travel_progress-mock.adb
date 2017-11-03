with Mock.Exceptions;

use Mock.Exceptions;

package body Active.Travel.Travel_Progress.Mock is

   function Create return Travel_Progress.Mock.Reference
   is (new Travel_Progress.Mock.Object);

   function Has_Next_Step (This   : in Travel_Progress.Mock.Object;
                           Travel : in Active.Travel.Object'Class)
                           return Boolean is
   begin
      if not This.Return_Values.Has_Next_Step_Existence then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception
           (Function_Name => "Has_Next_Step",
            Package_Name  => "Active.Travel.Travel_Progress");
      end if;

      return This.Return_Values.Has_Next_Step;
   end Has_Next_Step;

   function Is_Progressing (This   : in Travel_Progress.Mock.Object;
                            Travel : in Active.Travel.Object'Class)
                            return Boolean is
   begin
      if not This.Return_Values.Is_Progressing_Existence then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception
           (Function_Name => "Is_Progressing",
            Package_Name  => "Active.Travel.Travel_Progress");
      end if;

      return This.Return_Values.Is_Progressing;
   end Is_Progressing;

   procedure Set_Return_Value_For_Has_Next_Step (
      This         : in out Travel_Progress.Mock.Object;
      Return_Value : in Boolean) is
   begin
      This.Return_Values.Has_Next_Step := Return_Value;
      This.Return_Values.Has_Next_Step_Existence := TRUE;
   end Set_Return_Value_For_Has_Next_Step;

   procedure Set_Return_Value_For_Is_Progressing (
      This         : in out Travel_Progress.Mock.Object;
      Return_Value : in Boolean) is
   begin
      This.Return_Values.Is_Progressing := Return_Value;
      This.Return_Values.Is_Progressing_Existence := TRUE;
   end Set_Return_Value_For_Is_Progressing;

end Active.Travel.Travel_Progress.Mock;
