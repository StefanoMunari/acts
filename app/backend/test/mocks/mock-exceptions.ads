with Shared.String_Map;
with Shared.Infra_Id_To_String_Map;

package Mock.Exceptions is

   package String_Map             renames Shared.String_Map;
   package Infra_Id_To_String_Map renames Shared.Infra_Id_To_String_Map;

   Missing_Return_Value_For_Mocked_Procedure : exception;
   Missing_Return_Value_For_Mocked_Function : exception;

   procedure Raise_Missing_Return_Value_For_Mocked_Procedure_Exception (
      Procedure_Name : in String;
      Package_Name   : in String);

   procedure Raise_Missing_Return_Value_For_Mocked_Procedure_Exception (
      Parameter_Name : in String;
      Procedure_Name : in String;
      Package_Name   : in String);

   procedure Raise_Missing_Return_Value_For_Mocked_Procedure_Exception (
      Parameter_Name : in String;
      Procedure_Name : in String;
      Package_Name   : in String;
      Objects        : in String_Map.Map);

   procedure Raise_Missing_Return_Value_For_Mocked_Procedure_Exception (
      Procedure_Name : in String;
      Package_Name   : in String;
      Objects        : in String_Map.Map);

   procedure Raise_Missing_Return_Value_For_Mocked_Procedure_Exception (
      Parameter_Name : in String;
      Procedure_Name : in String;
      Package_Name   : in String;
      Objects        : in Infra_Id_To_String_Map.Map);

   procedure Raise_Missing_Return_Value_For_Mocked_Procedure_Exception (
      Procedure_Name : in String;
      Package_Name   : in String;
      Objects        : in Infra_Id_To_String_Map.Map);

   procedure Raise_Missing_Return_Value_For_Mocked_Function_Exception (
      Function_Name : in String;
      Package_Name  : in String);

   procedure Raise_Missing_Return_Value_For_Mocked_Function_Exception (
      Function_Name  : in String;
      Function_Param : in String;
      Package_Name   : in String);

   procedure Raise_Missing_Return_Value_For_Mocked_Function_Exception (
      Function_Name : in String;
      Package_Name  : in String;
      Objects       : in String_Map.Map);

   procedure Raise_Missing_Return_Value_For_Mocked_Function_Exception (
      Function_Name : in String;
      Package_Name  : in String;
      Objects       : in Infra_Id_To_String_Map.Map);

private
   function Extract_Objects_Data (
      Objects : in String_Map.Map)
   return String;

end Mock.Exceptions;
