with Interface_Layer.Presentation.Processors;
with Interface_Layer.Presentation.Splitters;
-- embed JSON_Converter
with Interface_Layer.Presentation.Converter.JSON;
-- DEBUG
-- DEBUG
package body Interface_Layer.Presentation is

   procedure Init is
   begin
      -- embed JSON_Converter
      Processors.Init (new Converter.JSON.Object);
      Splitters.Init;
   end Init;

   procedure Start is
   begin
      Processors.Start;
      Splitters.Start;
   end Start;

   procedure Shutdown is
   begin
      Processors.Shutdown;
   end Shutdown;

end Interface_Layer.Presentation;