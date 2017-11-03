package body Interface_Layer.Containers.Pair is

   function Create (First : String_Map.Data.Map; Second : Interface_Wrapper.Object)
   return Pair.Object is
      This : Pair.Object;
   begin
      This.First := First;
      This.Second := Second;
      return This;
   end Create;

end Interface_Layer.Containers.Pair;