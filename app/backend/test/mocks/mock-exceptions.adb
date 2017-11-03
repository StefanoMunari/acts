with Ada.Strings.Unbounded;
with Ada.Text_IO;

package body Mock.Exceptions is

   procedure Raise_Missing_Return_Value_For_Mocked_Procedure_Exception
     (Procedure_Name, Package_Name : in String) is
   begin
      raise Missing_Return_Value_For_Mocked_Procedure
        with "Missing set for value of procedure '" & Procedure_Name
        & "' of package '" & Package_Name & "'";
   end Raise_Missing_Return_Value_For_Mocked_Procedure_Exception;

   procedure Raise_Missing_Return_Value_For_Mocked_Procedure_Exception
     (Parameter_Name, Procedure_Name, Package_Name : in String) is
   begin
      raise Missing_Return_Value_For_Mocked_Procedure
        with "Missing set for value of parameter '" & Parameter_Name
        & "' of procedure '" & Procedure_Name
        & "' of package '" & Package_Name & "'";
   end Raise_Missing_Return_Value_For_Mocked_Procedure_Exception;

   procedure Raise_Missing_Return_Value_For_Mocked_Procedure_Exception
     (Parameter_Name, Procedure_Name, Package_Name : in String;
      Objects : in String_Map.Map) is
   begin
      raise Missing_Return_Value_For_Mocked_Procedure
        with "Missing set for value of parameter '" & Parameter_Name
        & "' of procedure '" & Procedure_Name
        & "' of package '" & Package_Name
        & Extract_Objects_Data (Objects) & "'";
   end Raise_Missing_Return_Value_For_Mocked_Procedure_Exception;

   procedure Raise_Missing_Return_Value_For_Mocked_Procedure_Exception
     (Procedure_Name, Package_Name : in String;
      Objects : in String_Map.Map) is
   begin
      raise Missing_Return_Value_For_Mocked_Function
        with "Missing set for value returned by procedure '" & Procedure_Name
        & "' of package '" & Package_Name & "'"
        & Extract_Objects_Data (Objects) & "'";
   end Raise_Missing_Return_Value_For_Mocked_Procedure_Exception;

   procedure Raise_Missing_Return_Value_For_Mocked_Procedure_Exception
     (Parameter_Name, Procedure_Name, Package_Name : in String;
      Objects : in Infra_Id_To_String_Map.Map) is
   begin
      raise Missing_Return_Value_For_Mocked_Procedure
        with "Missing set for value of parameter '" & Parameter_Name
        & "' of procedure '" & Procedure_Name
      -- TODO: expand map
        & "' of package '" & Package_Name & "' a map";
   end Raise_Missing_Return_Value_For_Mocked_Procedure_Exception;

   procedure Raise_Missing_Return_Value_For_Mocked_Procedure_Exception
     (Procedure_Name, Package_Name : in String;
      Objects : in Infra_Id_To_String_Map.Map) is
   begin
      raise Missing_Return_Value_For_Mocked_Function
        with "Missing set for value returned by procedure '" & Procedure_Name
        & "' of package '" & Package_Name & "'"
      -- TODO: expand map
        & "a map";
   end Raise_Missing_Return_Value_For_Mocked_Procedure_Exception;

   procedure Raise_Missing_Return_Value_For_Mocked_Function_Exception
     (Function_Name, Package_Name : in String) is
   begin
      raise Missing_Return_Value_For_Mocked_Function
        with "Missing set for value returned by function '" & Function_Name
        & "' of package '" & Package_Name & "'";
   end Raise_Missing_Return_Value_For_Mocked_Function_Exception;

   procedure Raise_Missing_Return_Value_For_Mocked_Function_Exception
     (Function_Name, Function_Param, Package_Name : in String) is
   begin
      raise Missing_Return_Value_For_Mocked_Function
        with "Missing set for value returned by function '" & Function_Name
        & "' of package '" & Package_Name
        & "' passing parameter '" & Function_Param & "'";
   end Raise_Missing_Return_Value_For_Mocked_Function_Exception;

   procedure Raise_Missing_Return_Value_For_Mocked_Function_Exception
     (Function_Name, Package_Name : in String;
      Objects : in String_Map.Map) is
   begin
      raise Missing_Return_Value_For_Mocked_Function
        with "Missing set for value returned by function '" & Function_Name
        & "' of package '" & Package_Name & "'"
        & Extract_Objects_Data (Objects) & "'";
   end Raise_Missing_Return_Value_For_Mocked_Function_Exception;

   procedure Raise_Missing_Return_Value_For_Mocked_Function_Exception
     (Function_Name, Package_Name : in String;
      Objects : in Infra_Id_To_String_Map.Map) is
   begin
      raise Missing_Return_Value_For_Mocked_Function
        with "Missing set for value returned by function '" & Function_Name
      -- TODO: expand map
        & "' of package '" & Package_Name & "'" & "' a map";
   end Raise_Missing_Return_Value_For_Mocked_Function_Exception;

   function Extract_Objects_Data
     (Objects : in String_Map.Map)
      return String
   is
      Objects_Data : Ada.Strings.Unbounded.Unbounded_String;
   begin
      Ada.Text_IO.Put_Line ("Loop");
      for Cursor in Objects.Iterate
      loop
         Ada.Strings.Unbounded.Append
           (Objects_Data,
            (if String_Map."=" (Cursor, Objects.First)
             then " for " else " and ")
            & String_Map.Element (Cursor) & " with id"
            & Natural'Image (String_Map.Key (Cursor)));
      end loop;

      return Ada.Strings.Unbounded.To_String (Objects_Data);
   end Extract_Objects_Data;

end Mock.Exceptions;
