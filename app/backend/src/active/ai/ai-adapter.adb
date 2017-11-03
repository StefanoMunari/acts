-- PRE: generate ada spec of the ai_interface header
-- g++ -c -fdump-ada-spec ai_interface.h
with ai_interface_h;

with AI.Adapter.Exceptions;

with Ada.Unchecked_Deallocation;

with Interfaces.C.Strings;
with System.Address_to_Access_Conversions;
with System.Address_Image;

package body AI.Adapter is

   package AI_Interface renames ai_interface_h;
   package AI_Exc renames AI.Adapter.Exceptions;
   package C_Strings renames Interfaces.C.Strings;

   procedure Set_Clients_Limit (
            This               : in AI.Adapter.Object;
            New_Clients_Limit  :    Natural)
   is
      C_Clients_Limit   : Interfaces.C.int :=
         Interfaces.C.int (Integer (New_Clients_Limit));
      Result : Boolean := True;
   begin
      if Clients_Limit = 0 and New_Clients_Limit > 0 then
         Result := Boolean'Val (
            AI_Interface.Set_Clients_Limit (
               C_Clients_Limit));
         Clients_Limit := New_Clients_Limit;
      end if;
      if Result = False then
         AI_Exc.Raise_Limit_Exception;
      end if;
   end Set_Clients_Limit;

   procedure Init (This           : in out AI.Adapter.Object;
                   Clients_Limit  :        Natural;
                   Data_Path      :        String;
                   File_Prefix    :        String;
                   File_Extension :        String;
                   Agent_Id       :        String)
   is
      C_AI_Id          : C_Strings.chars_ptr :=
         C_Strings.New_String (File_Prefix);
      C_Agent_Id       : C_Strings.chars_ptr :=
         C_Strings.New_String (Agent_Id);
      C_Data_Path      : C_Strings.chars_ptr :=
         C_Strings.New_String (Data_Path);
      C_File_Prefix    : C_Strings.chars_ptr :=
         C_Strings.New_String (File_Prefix);
      C_File_Extension : C_Strings.chars_ptr :=
         C_Strings.New_String (File_Extension);
      -- The result of the AI invocations
      -- (when no other return type is provided)
      --    - False
      --      indicates that something goes wrong => raise an Ada exception
      --    - True
      --      the invocation completed successfully
      Result : Boolean;
   begin
      -- Set the Max_Number_Of_Clients for AI
      This.Set_Clients_Limit (Clients_Limit);
      -- Init the AI_Interface instance
      Result := Boolean'Val (AI_Interface.Init (
         C_AI_Id, C_Agent_Id, C_Data_Path, C_File_Prefix, C_File_Extension));
      if Result = False then
         AI_Exc.Raise_Init_Exception;
      end if;
      -- Free Resources
      C_Strings.Free (C_AI_Id);
      C_Strings.Free (C_Agent_Id);
      C_Strings.Free (C_Data_Path);
      C_Strings.Free (C_File_Prefix);
      C_Strings.Free (C_File_Extension);
      null;
   end Init;

   function Find_Path (This        : in AI.Adapter.Object;
                       Source      :    String;
                       Destination :    String;
                       Algorithm   :    Natural;
                       S_Type      : in String;
                       Agent_Id    : in String)
      return AI.Step_List.List
   is
      Path_Size     : Integer;
      Path          : AI.Step_List.List := AI.Step_List.Empty_List;
      C_AI_Id       : C_Strings.chars_ptr := C_Strings.New_String (S_Type);
      C_Agent_Id    : C_Strings.chars_ptr := C_Strings.New_String (Agent_Id);
      C_Source      : C_Strings.chars_ptr := C_Strings.New_String (Source);
      C_Destination : C_Strings.chars_ptr :=
         C_Strings.New_String (Destination);
      C_Algorithm   : Interfaces.C.int :=
         Interfaces.C.int (Integer (Algorithm));
      -- The result of the AI invocations
      -- (when no other return type is provided)
      --    - False
      --      indicates that something goes wrong => raise an Ada exception
      --    - True
      --      the invocation completed successfully
      Result : Boolean;
   begin
      -- Find Path
      -- from Source to Destination using the specified Algorithm
      Result := Boolean'Val (AI_Interface.Find
         (C_AI_Id, C_Agent_Id, C_Source, C_Destination, C_Algorithm));
      if Result = False then
         AI_Exc.Raise_Find_Exception;
      end if;
      -- -- Free resources
      C_Strings.Free (C_Source);
      C_Strings.Free (C_Destination);
      -- -- Get the found Path_Size
      Path_Size := Integer (AI_Interface.Get_Path_Size (C_AI_Id, C_Agent_Id));
      -- Get the found Path
      declare
         type C_String_Array is
            array (1 .. Path_Size) of C_Strings.chars_ptr;
         type Ref_C_String_Array is access all C_String_Array;
            procedure Free_C_Array is new
               Ada.Unchecked_Deallocation (C_String_Array, Ref_C_String_Array);
         package Ref_CString_Conversion is
            new System.Address_to_Access_Conversions
               (Object => C_String_Array);
         C_Path : Ref_C_String_Array;
      begin
         C_Path :=
            Ref_C_String_Array
            (Ref_CString_Conversion.To_Pointer
            (AI_Interface.Get_Path (C_AI_Id, C_Agent_Id)));
         for Index in 1 .. Path_Size loop
            -- append a String ID
            Path.Append (C_Strings.Value (C_Path (Index)));
         end loop;
         for Index in Path_Size .. 1 loop
            C_Strings.Free (C_Path (Index));
         end loop;
         Free_C_Array (C_Path);
      end;
      -- Free resources
      C_Strings.Free (C_AI_Id);
      C_Strings.Free (C_Agent_Id);
      -- return the found Path
      return Path;
   end Find_Path;

   procedure Finalize (This : in out AI.Adapter.Object)
   is
      -- The result of the AI invocations
      -- (when no other return type is provided)
      --    - False
      --      indicates that something goes wrong => raise an Ada exception
      --    - True
      --      the invocation completed successfully
      Result : Boolean;
   begin
      Result := Boolean'Val (AI_Interface.Finalize);
      if Result = False then
         AI_Exc.Raise_Finalize_Exception;
      end if;
   end Finalize;

end AI.Adapter;