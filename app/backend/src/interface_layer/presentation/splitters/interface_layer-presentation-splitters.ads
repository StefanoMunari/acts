with Shared.Process_Types;

package Interface_Layer.Presentation.Splitters is

   package PT renames Shared.Process_Types;

   procedure Init;
   procedure Start;
   procedure Shutdown;

private

   function Is_Shutdown (Action : String) return Boolean;

   task type Splitter is
      entry Split;
   end Splitter;
   type Reference is access Splitter;

   -- static data fields
   Splitter_State : PT.Process_T := PT.TERMINATED;
   Splitter_Ref : Reference := NULL;

end Interface_Layer.Presentation.Splitters;
