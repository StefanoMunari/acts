with Mock.Exceptions;

use Mock.Exceptions;

package body Active.Travel.Travel_State.Mock is

   function Create return access Travel_State.Mock.Object
   is (new Travel_State.Mock.Object);

   function Has_Next_Step (This   : in Travel_State.Mock.Object;
                           Travel : in Active.Travel.Object'Class)
   return Boolean is
   begin
      if not This.Return_Values.Has_Next_Step_Existence then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception
           (Function_Name => "Has_Next_Step",
            Package_Name  => "Active.Travel.Travel_State.Mock");
      end if;

      return This.Return_Values.Has_Next_Step;
   end Has_Next_Step;

   function Is_Progressing (This   : in Travel_State.Mock.Object;
                            Travel : in Active.Travel.Object'Class)
   return Boolean is
   begin
      if not This.Return_Values.Is_Progressing_Existence then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception
           (Function_Name => "Is_Progressing",
            Package_Name  => "Active.Travel.Travel_State.Mock");
      end if;

      return This.Return_Values.Is_Progressing;
   end Is_Progressing;

   function Dump (This : Travel_State.Mock.Object) return SU.Unbounded_String
   is
   begin
      if not This.Return_Values.Dump_Existence then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception
           (Function_Name => "Dump",
            Package_Name  => "Active.Travel.Travel_State.Mock");
      end if;

      return This.Return_Values.Dump;
   end Dump;

   procedure Set_Return_Value_For_Has_Next_Step (
      This         : in out Travel_State.Mock.Object;
      Return_Value : in     Boolean) is
   begin
      This.Return_Values.Has_Next_Step := Return_Value;
      This.Return_Values.Has_Next_Step_Existence := TRUE;
   end Set_Return_Value_For_Has_Next_Step;

   procedure Set_Return_Value_For_Is_Progressing (
      This         : in out Travel_State.Mock.Object;
      Return_Value : in     Boolean) is
   begin
      This.Return_Values.Is_Progressing := Return_Value;
      This.Return_Values.Is_Progressing_Existence := TRUE;
   end Set_Return_Value_For_Is_Progressing;

   procedure Set_Return_Value_For_Dump (
      This         : in out Travel_State.Mock.Object;
      Return_Value : in     SU.Unbounded_String) is
   begin
      This.Return_Values.Dump := Return_Value;
      This.Return_Values.Dump_Existence := TRUE;
   end Set_Return_Value_For_Dump;

end Active.Travel.Travel_State.Mock;
