with Interface_Layer.Presentation.Converter;

with Shared.Process_Types;

package Interface_Layer.Presentation.Processors is
   package PT renames Shared.Process_Types;
   package Base_Converter renames Interface_Layer.Presentation.Converter;

   procedure Init (Format_Reference : Base_Converter.Reference);
   procedure Start;
   procedure Shutdown;

private
   -- static data fields
   Format_Converter : Base_Converter.Reference;

end Interface_Layer.Presentation.Processors;
