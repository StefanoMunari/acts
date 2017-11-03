with AI.Translator;

package body Active.Build.AI.Config_Reader is

   package Translator renames Standard.AI.Translator;

   procedure Read_Config (This      : in Config_Reader.Object;
                          File_Name : in String)
   is
   begin
      Translator.Translate_To_Infrastructure (File_Name);
      Translator.Translate_To_Costs (File_Name);
   end Read_Config;

end Active.Build.AI.Config_Reader;
