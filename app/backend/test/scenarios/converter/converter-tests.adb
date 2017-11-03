with AUnit.Assertions;
with Converter;
with Converter.JSON;
with Active;
with GNATCOLL.JSON;
with Ada.Strings.Unbounded;

package body Converter.Tests is
   package Ass renames AUnit.Assertions;
   package G_JSON renames GNATCOLL.JSON;
   package CJ renames Converter.JSON;
   package SU renames Ada.Strings.Unbounded;

   -----------------------------------------------------
   --                   SCAFFOLDING
   -----------------------------------------------------
   Conv : Converter.MsgConverter_Ref;
   Entity : Active.MovingEntity_Ref;
   Message : G_JSON.JSON_Value;
   Expected_Entity : Active.MovingEntity_Ref;
   Expected_Message : G_JSON.JSON_Value;
   Unexpected_Entity : Active.MovingEntity_Ref;
   Unexpected_Message : G_JSON.JSON_Value;
   
   procedure Set_Up_Case (T: in out Converter_Test) is
      pragma Unreferenced (T);
   begin
      Expected_Message := G_JSON.Create_Object;
      G_JSON.Set_Field (Expected_Message,
         Field_Name => "id", 
         Field => "Test");
      G_JSON.Set_Field (Expected_Message,
         Field_Name => "maxSpeed",
         Field => Natural(100));
      Expected_Entity := new Active.Moving_Entity;
      Expected_Entity := Active.Create 
         (SU.To_Unbounded_String ("Test"), 100);
      Unexpected_Entity := Active.Create 
         (SU.To_Unbounded_String ("Wrong"), 0);
      Unexpected_Message := G_JSON.Create_Object;
      G_JSON.Set_Field (Unexpected_Message,
         Field_Name => "id", 
         Field => "Wrong");
      G_JSON.Set_Field (Unexpected_Message,
         Field_Name => "maxSpeed",
         Field => Natural(0));
   end Set_Up_Case;

   procedure Set_Up (T: in out Converter_Test) is
      pragma Unreferenced (T);
   begin
      Conv := new CJ.JSON_Message_Converter;
      Entity := Active.Create 
         (SU.To_Unbounded_String ("Test"), 100);
      Message := G_JSON.Create_Object;
      G_JSON.Set_Field (Message,
         Field_Name => "id", 
         Field => "Test");
      G_JSON.Set_Field (Message,
         Field_Name => "maxSpeed",
         Field => Natural(100));
   end Set_Up;

   procedure Tear_Down (T: in out Converter_Test) is
      pragma Unreferenced (T);
   begin
      null;
   end Tear_Down;

   procedure Tear_Down_Case (T: in out Converter_Test) is
      pragma Unreferenced (T);
   begin
      Expected_Message := G_JSON.Read ("{}");
      Unexpected_Message := G_JSON.Read ("{}"); 
   end Tear_Down_Case;

   -----------------------------------------------------
   --                   SCENARIOS
   -----------------------------------------------------
   procedure Encode (T: in out TC.Test_Case'Class) is
      Result : G_JSON.JSON_Value;
   begin
      Result := CJ.Encode (Conv, Entity);
      Ass.Assert ((G_JSON.Write (Result)) = (G_JSON.Write (Expected_Message)), 
      "Wrong Encoding - Expected Message");
      Ass.Assert ((G_JSON.Write (Result)) /= (G_JSON.Write (Unexpected_Message)), 
      "Wrong Encoding - Wrong Message");
   end Encode;

   procedure Decode (T: in out TC.Test_Case'Class) is
      Result : Active.MovingEntity_Ref;
   begin
      Result := CJ.Decode (Conv, Message);
      Ass.Assert ( (Active.Get_Id (Result) = Active.Get_Id (Expected_Entity)),
       "Wrong Id Decoding - Expected_Entity");
      Ass.Assert ( (Active.Get_Max_Speed (Result) = 
         Active.Get_Max_Speed (Expected_Entity)), 
      "Wrong Max_Speed Decoding - Expected_Entity");
      Ass.Assert ( (Active.Get_Max_Speed (Result) /= 
         Active.Get_Max_Speed (Unexpected_Entity)), 
      "Wrong Id Decoding - Wrong Entity");
      Ass.Assert ( (Active.Get_Max_Speed (Result) /= 
         Active.Get_Max_Speed (Unexpected_Entity)), 
      "Wrong Max_Speed Decoding - Wrong Entity");
   end Decode;
   -----------------------------------------------------
   --                  REGISTRATION 
   -----------------------------------------------------
   procedure Register_Tests (T: in out Converter_Test) is
      use TC.Registration;
   begin
      Register_Routine (Test => T, 
                        Routine => Encode'Access, 
                        Name => "Encode");
      Register_Routine (Test => T, 
                        Routine => Decode'Access, 
                        Name => "Decode");
   end Register_Tests;

   function Name(T: Converter_Test) return AU.Message_String is
      pragma Unreferenced (T);
   begin
      return AU.Format ("Converter");
   end Name;
end Converter.Tests;
