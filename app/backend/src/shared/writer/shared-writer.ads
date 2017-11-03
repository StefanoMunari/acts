-- library
with GNATCOLL.JSON;

package Shared.Writer is
   package G_JSON renames GNATCOLL.JSON;

   type Object is interface;
   type Reference is access all Writer.Object'Class;

   procedure Write (This : in Writer.Object;
   				   File_Path : in String;
   				   Input_Data : in G_JSON.JSON_Value)
   is abstract;

end Shared.Writer;
