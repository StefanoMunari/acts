separate (Interface_Layer.Presentation.Converter.JSON)

function Parse_Key (This      : in JSON.Object;
                    Raw_Key   : in String;
                    Separator : in String)
return String is
   Position : Positive;
begin
   Position := Positive (SFix.Index (Raw_Key, Separator));
   return SFix.Delete (Raw_Key, Position, Raw_Key'Last);
end Parse_Key;
