package body Interface_Layer.Tables.Pending_Request is

   protected body Table is

      procedure Add (
         Key   : in String;
         Value : in Callback_Pair_Pkg.Object) is
      begin
         if not Pending.Contains (Key) then
            Pending.Include (Key, Value);
         end if;
      end Add;

      function Contains (Key : in String)
      return Boolean is
      begin
         return Pending.Contains (Key);
      end Contains;

      function Find (Key : in String)
      return Callback_Pair_Pkg.Object is
      begin
         return Pending.Element (Key);
      end Find;

      procedure Delete (
         Key   : in     String;
         Found :    out Boolean) is
      begin
         Pending.Exclude (Key);
      end Delete;

      procedure Find_And_Delete (
         Key   : in     String;
         Pair  :    out Callback_Pair_Pkg.Object;
         Found :    out Boolean)
      is
      begin
         if not Pending.Contains (Key) then
            Found := False;
            return;
         end if;
         Pair  := Pending.Element (Key);
         Found := True;
         Pending.Delete (Key);
      end Find_And_Delete;

      procedure Clear is
      begin
         Pending.Clear;
      end Clear;

   end Table;

end Interface_Layer.Tables.Pending_Request;
