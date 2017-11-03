separate (Interface_Layer.Presentation.Splitters)
procedure Start is
begin
   Splitter_State := PT.ACTIVE;
   Splitter_Ref.all.Split;
end Start;
