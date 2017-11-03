-- library
with GNATCOLL.JSON;

package Shared.Writer.JSON is
   package G_JSON renames GNATCOLL.JSON;

   type Object is new Writer.Object with null record;
   type Reference is access all JSON.Object'Class;

   overriding
   procedure Write (This : in Writer.JSON.Object;
   				   File_Path : in String;
   				   Input_Data : in G_JSON.JSON_Value);
end Shared.Writer.JSON;
