with GNATCOLL.JSON;

package Shared.JSON_Array_Utils is

   package G_JSON renames GNATCOLL.JSON;

   type JSON_Array_Ref is access all G_JSON.JSON_Array;

   function Equals (A,B : JSON_Array_Ref) return Boolean; 
   
end Shared.JSON_Array_Utils;