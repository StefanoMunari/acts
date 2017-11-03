with AUnit.Assertions;
with Ada.Text_IO;
with Procedure_Call;
with Ada.Containers.Hashed_Maps;
with Ada.Strings;

package body Procedure_Call.Tests is
   package Ass renames AUnit.Assertions;
   package IO renames Ada.Text_IO;
   package PC renames Procedure_Call;
   package AC renames Ada.Containers;
   package AS renames Ada.Strings;
   -----------------------------------------------------
   --                   SCAFFOLDING
   -----------------------------------------------------
   ProcID0 : PC.Procedure_ID;
   ProcID1 : PC.Procedure_ID;   
   ID0_Hash : AC.Hash_Type;
   ID1_Hash : AC.Hash_Type;

   procedure Set_Up_Case (T: in out Procedure_Call_Test) is
      pragma Unreferenced (T);
   begin
      ID0_Hash := AC.Hash_Type (PC.INSERT_ENTITY);
      ID1_Hash := AC.Hash_Type (PC.QUERY); 
   end Set_Up_Case;

   procedure Set_Up (T: in out Procedure_Call_Test) is
      pragma Unreferenced (T);
   begin
      ProcID0 := PC.INSERT_ENTITY;
      ProcID1 := PC.QUERY;
   end Set_Up;

   procedure Tear_Down (T: in out Procedure_Call_Test) is
      pragma Unreferenced (T);
   begin
      IO.Put_Line ("Tear_Down");
   end Tear_Down;

   procedure Tear_Down_Case (T: in out Procedure_Call_Test) is
      pragma Unreferenced (T);
   begin
      IO.Put_Line ("Tear_Down_Case");
   end Tear_Down_Case;

   -----------------------------------------------------
   --                   SCENARIOS
   -----------------------------------------------------
   procedure ID_Hashed (T: in out TC.Test_Case'Class) is
      Result : AC.Hash_Type;
   begin
      Result := PC.ID_Hashed (ProcID0);
      Ass.Assert (Result = ID0_Hash, "Wrong Hashing");
      Result := PC.ID_Hashed (ProcID1);
      Ass.Assert (Result = ID1_Hash,  "Wrong Hashing");
   end ID_Hashed;
   -----------------------------------------------------
   --                  REGISTRATION 
   -----------------------------------------------------
   procedure Register_Tests (T: in out Procedure_Call_Test) is
      use TC.Registration;
   begin
      Register_Routine (Test => T, 
                        Routine => ID_Hashed'Access, 
                        Name => "ID_Hashed");
   end Register_Tests;

   function Name(T: Procedure_Call_Test) return AU.Message_String is
      pragma Unreferenced (T);
   begin
      return AU.Format ("Procedure_Call");
   end Name;
end Procedure_Call.Tests;
