with Ada.Exceptions;
with Ada.Strings.Unbounded;
with Ada.Text_IO;


package body Shared.Reader.JSON is
   package Exc renames Ada.Exceptions;
   package SU renames Ada.Strings.Unbounded;
   package IO renames Ada.Text_IO;

   function Parse (This : Reader.JSON.Object; File_Path : in String)
   return G_JSON.JSON_Value
   is
      Result  : SU.Unbounded_String;
      FileObj : IO.File_Type;
   begin
      Result := SU.To_Unbounded_String("");
      IO.Open (File => FileObj, Mode => IO.In_File, Name => File_Path);
      Read_Line_By_Line:
      while not IO.End_Of_File (FileObj) loop
         Result := SU.To_Unbounded_String (
            SU.To_String (Result) & IO.Get_Line (FileObj));
      end loop Read_Line_By_Line;
      IO.Close (FileObj);
      return G_JSON.Read (SU.To_String (Result), "Reader_JSON.errors");

      -- handle exceptions
      exception when E : others =>
         IO.Put_Line (
            Exc.Exception_Name (E) & ": " & Exc.Exception_Message (E));
         return G_JSON.Create_Object;
   end Parse;

end Shared.Reader.JSON;


