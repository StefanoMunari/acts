------------------------------------------------------------------------------
-- @author  <gianmarco.midena@gmail.com>
-- @author  <stefanomunari.sm@gmail.com>
-- @author  <valle.sebastiano93@gmail.com>
-- @context application-backend::interface_layer-tables-pending_request
-- @purpose Acts as a registry for Id -> <Success, Failure> maps
-- @interface Add (String, Pair):
--              Creates entry
--            Contains (String) -> Boolean:
--              true iff key is in map
--            Find (String) -> Pair:
--              returns a callback pair, given the key
--            Delete (String):
--              deletes entry in map, if any
--            Find_And_Delete (String) -> <Pair, Boolean>:
--              combines Find and Delete
--            Clear:
--              clears the table
-- @dependencies application-backend::shared-callback_map
--               application-backend::shared-callback-callback_pair
-- @details Singleton
------------------------------------------------------------------------------

with Shared.Callback_Map;
with Shared.Callback_Pair;

-- Singleton
package Interface_Layer.Tables.Pending_Request is

   package Callback_Map_Pkg  renames Shared.Callback_Map;
   package Callback_Pair_Pkg renames Shared.Callback_Pair;

   protected Table is

      procedure Add (
         Key   : in String;
         Value : in Callback_Pair_Pkg.Object);

      function Contains (Key : in String)
      return Boolean;

      function Find (Key : in String)
      return Callback_Pair_Pkg.Object;

      procedure Delete (Key : in String; Found : out Boolean);

      procedure Find_And_Delete (
         Key   : in     String;
         Pair  :    out Callback_Pair_Pkg.Object;
         Found :    out Boolean);

      procedure Clear;

   private
      Pending : Callback_Map_Pkg.Map := Callback_Map_Pkg.Empty_Map;
   end Table;

end Interface_Layer.Tables.Pending_Request;
