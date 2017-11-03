-- local
with Shared.JSON_Splitter;
with Shared.String_List;
-- core
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;
-- library
with GNAT.String_Split;
with GNATCOLL.JSON;
-- DEBUG
-- DEBUG

package body Interface_Layer.Presentation.Converter.JSON is

   package JSON_Splitter renames Shared.JSON_Splitter;
   package String_List renames Shared.String_List;
   package SU renames Ada.Strings.Unbounded;
   package SFix renames Ada.Strings.Fixed;
   package G_JSON renames GNATCOLL.JSON;

   function Encode (This : in JSON.Object; Raw : in String_Map.Data.Map)
      return XFormat.Object'Class is separate;

   function Decode (This : in JSON.Object; Message : in XFormat.Object'Class)
      return String_Map.Data.Map is separate;

-- private
   procedure Parse_Raw (This        : in     JSON.Object;
                        Pair        : in     String_Map.Data.Cursor;
                        Separator   : in     String;
                        Accumulator : in out JSON_Array_Ref_HMap.Map)
   is separate;

  function Parse_Key (This      : in JSON.Object;
                      Raw_Key   : in String;
                      Separator : in String)
   return String is separate;

  function Parse_List (This      : in JSON.Object;
                       Raw_List  : in String;
                       Separator : in String)
   return Utils.JSON_Array_Ref is separate;

end Interface_Layer.Presentation.Converter.JSON;
