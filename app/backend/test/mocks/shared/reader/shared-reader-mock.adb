with Mock.Exceptions;

use Mock.Exceptions;

package body Shared.Reader.Mock is

   function Create return Reader.Mock.Reference
   is (new Shared.Reader.Mock.Object);

   function Parse (This : Reader.Mock.Object; File_Path : in String)
      return G_JSON.JSON_Value
   is
   begin
      if not This.Return_Values.Parse_Existence then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception
           (Function_Name  => "Parse",
            Function_Param => "Return value",
            Package_Name   => "Shared.Reader.Mock");
      end if;

      return This.Return_Values.Parse;
   end Parse;

   procedure Set_Return_Value_For_Parse (
      This         : in out Reader.Mock.Object;
      Return_Value : in     G_JSON.JSON_Value) is
   begin
      This.Return_Values.Parse := Return_Value;
      This.Return_Values.Parse_Existence := True;
   end Set_Return_Value_For_Parse;

end Shared.Reader.Mock;
