-- core
with Ada.Strings.Unbounded;
-- gnatcoll libs
with GNATCOLL.JSON;

--local
with Active.Agent;

with Reactive;

package Interface_Layer.Remote.Query_Decoder is

   package G_JSON renames GNATCOLL.JSON;
   package Agent  renames Active.Agent;
   package SU     renames Ada.Strings.Unbounded;

   use Reactive.Infra_Id_Type;

   type Object is tagged private;
   type Reference is access all Query_Decoder.Object'Class;

-- Create decoder from a string representing a procedure invocation
   function Create (Str : in String) return Query_Decoder.Reference;

   function Decode_Correlation_Id (This : Query_Decoder.Object)
   return Agent.Agent_Id;

   function Decode_Name (This : Query_Decoder.Object) return String;

   function Decode_Arg (
      This  :    Query_Decoder.Object;
      Index : in Positive) return Infra_Id;

   function Decode_Arg (
      This  :    Query_Decoder.Object;
      Index : in Positive) return Agent.Agent_Id;

   function Decode_Arg (
      This  :    Query_Decoder.Object;
      Index : in Positive) return Integer;

-- add here your decoding for other types...

private

   function Clean_Agent_Id (Value_SU : SU.Unbounded_String)
   return Agent.Agent_Id;

   type Object is tagged record
      Query_Object : G_JSON.JSON_Value;
   end record;

end Interface_Layer.Remote.Query_Decoder;
