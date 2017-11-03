with Ada.Exceptions;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO;


package body Shared.Writer.JSON is
   package Exc renames Ada.Exceptions;
   package SU renames Ada.Strings.Unbounded;
   package IO renames Ada.Text_IO;

   procedure Write(This : in Writer.JSON.Object;
                  File_Path : in String;
                  Input_Data : in G_JSON.JSON_Value)
   is
      Output : IO.File_Type;
      Data : SU.Unbounded_String := G_JSON.Write (Input_Data);
   begin
      IO.Open (File => Output, Mode => IO.Out_File, Name => File_Path);
      IO.Unbounded_IO.Put_Line (Output, Data);
      IO.Close (Output);

      -- handle exceptions
      exception when E : others =>
         IO.Put_Line
         (Exc.Exception_Name (E) & ": " & Exc.Exception_Message (E));
   end Write;

end Shared.Writer.JSON;