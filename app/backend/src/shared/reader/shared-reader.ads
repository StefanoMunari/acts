-- library
with GNATCOLL.JSON;

package Shared.Reader is
   package G_JSON renames GNATCOLL.JSON;

   type Object is interface;
   type Reference is access all Reader.Object'Class;

   -- JSON CONSTANTS FIELDS
   Nodes_Field : constant String := "nodes";

   function Parse (This : Reader.Object; File_Path : in String)
      return G_JSON.JSON_Value is abstract;

end Shared.Reader;
