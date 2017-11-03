with Mock.Exceptions;
use Mock.Exceptions;

package body AI.Mock is

   procedure Init (This           : in out AI.Mock.Object;
                   Clients_Limit  :        Natural;
                   Data_Path      :        String;
                   File_Prefix    :        String;
                   File_Extension :        String;
                   Agent_Id       :        String)
   is
   begin
      This.Mock_Values.Init_Called := True;
   end Init;

   function Find_Path (
            This        : in AI.Mock.Object;
            Source      :    String;
            Destination :    String;
            Algorithm   :    Natural;
            S_Type      : in String;
            Agent_Id    : in String)
   return AI.Step_List.List is
   begin
      if not This.Return_Values.Find_Path_Existence then
         Raise_Missing_Return_Value_For_Mocked_Procedure_Exception (
            Procedure_Name => "Find_Path",
            Package_Name   => "AI.Mock");
      end if;

      return This.Return_Values.Find_Path;
   end Find_Path;

   function Get_Init_Called (This : AI.Mock.Object) return Boolean
   is (This.Mock_Values.Init_Called);

   procedure Set_Return_Value_For_Find_Path (
      This         : in out AI.Mock.Object;
      Return_Value : in     AI.Step_List.List) is
   begin
      This.Return_Values.Find_Path := Return_Value;
      This.Return_Values.Find_Path_Existence := True;
   end Set_Return_Value_For_Find_Path;

end AI.Mock;
