-- library
with GNATCOLL.JSON;

package Shared.Reader.Mock is
   package G_JSON renames GNATCOLL.JSON;

   type Object is new Reader.Object with private;
   type Reference is access all Reader.Mock.Object'Class;

   function Create return Reader.Mock.Reference;

   overriding
   function Parse (This : Reader.Mock.Object; File_Path : in String)
      return G_JSON.JSON_Value;

   not overriding
   procedure Set_Return_Value_For_Parse (
      This         : in out Reader.Mock.Object;
      Return_Value : in     G_JSON.JSON_Value);

private
   type Return_Values_Collection is record
      Parse : G_JSON.JSON_Value;
      Parse_Existence : Boolean := FALSE;
   end record;

   type Object is new Reader.Object with record
      Return_Values : Return_Values_Collection;
   end record;

end Shared.Reader.Mock;
