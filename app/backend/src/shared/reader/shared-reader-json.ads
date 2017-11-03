-- library
with GNATCOLL.JSON;

package Shared.Reader.JSON is
   package G_JSON renames GNATCOLL.JSON;

   type Object is new Reader.Object with null record;
   type Reference is access all JSON.Object'Class;

   overriding
   function Parse (This : Reader.JSON.Object; File_Path : in String)
      return G_JSON.JSON_Value;
end Shared.Reader.JSON;
