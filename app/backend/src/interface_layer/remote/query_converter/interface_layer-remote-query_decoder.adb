with Ada.Strings.Fixed;
with Ada.Text_IO;

package body Interface_Layer.Remote.Query_Decoder is

   package SFix   renames Ada.Strings.Fixed;

   function Create (Str : in String) return Query_Decoder.Reference
   is
      This : Query_Decoder.Reference := new Query_Decoder.Object;
   begin
      This.Query_Object := G_JSON.Read (Str);
      return This;
   end Create;

   function Decode_Correlation_Id (This : Query_Decoder.Object)
   return Agent.Agent_Id
   is
      Correlation_Id : SU.Unbounded_String;
   begin
      Correlation_Id := This.Query_Object.Get (Remote.Query_Corr_Field);
      return Clean_Agent_Id (Correlation_Id);
   end Decode_Correlation_Id;

   function Decode_Name (This : Query_Decoder.Object) return String
   is
   begin
      return This.Query_Object.Get (Remote.Query_Proc_Field);
   end Decode_Name;

   function Decode_Arg (
      This  :    Query_Decoder.Object;
      Index : in Positive)
   return Infra_Id
   is
      Args      : G_JSON.JSON_Array;
      Value_Int : Integer;
   begin
      Args := This.Query_Object.Get (Remote.Query_Args_Field);
      Value_Int := G_JSON.Get (Args, Index).Get;
      return Infra_Id (Natural (Value_Int));
   end Decode_Arg;

   function Decode_Arg (
      This  :    Query_Decoder.Object;
      Index : in Positive)
   return Agent.Agent_Id
   is
      Args     : G_JSON.JSON_Array;
      Value_SU : SU.Unbounded_String;
   begin
      Args := This.Query_Object.Get (Remote.Query_Args_Field);
      Value_SU := G_JSON.Get (Args, Index).Get;
      return Clean_Agent_Id (Value_SU);
   end Decode_Arg;

   function Decode_Arg (
      This  :    Query_Decoder.Object;
      Index : in Positive)
   return Integer
   is
      Args      : G_JSON.JSON_Array;
      Value_Int : Integer;
   begin
      Args := This.Query_Object.Get (Remote.Query_Args_Field);
      Value_Int := G_JSON.Get (Args, Index).Get;
      return Value_Int;
   end Decode_Arg;

   function Clean_Agent_Id (Value_SU : SU.Unbounded_String)
   return Agent.Agent_Id
   is
      Polish_SU : SU.Unbounded_String := SU.To_Unbounded_String ("");
      Result    : Agent.Agent_Id;
   begin
      for I in 1 .. SU.Length (Value_SU) loop
         if SU.Element (Value_SU, I) = '"' then
            null;
         else
            SU.Append (Polish_SU, SU.Element (Value_SU, I));
         end if;
      end loop;
      Result :=
         Agent.Create_Id_From_Natural (
            Integer'Value (Su.To_String (Polish_SU)));
      return Result;
   end Clean_Agent_Id;

end Interface_Layer.Remote.Query_Decoder;
