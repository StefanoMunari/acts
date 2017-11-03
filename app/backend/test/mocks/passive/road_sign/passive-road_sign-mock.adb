with Mock.Exceptions; use Mock.Exceptions;

package body Passive.Road_Sign.Mock is

   function Create return Road_Sign.Reference
   is (new Passive.Road_Sign.Mock.Object);

   procedure Apply (This      : in out Road_Sign.Mock.Object;
                    Traveller : in     Agent.Agent_Id) is
   begin
      This.Applied_For.Include (Traveller);
   end Apply;

   overriding
   function Dump (This : Road_Sign.Mock.Object)
   return G_JSON.JSON_Value is
   begin
      if not This.Return_Values.Dump_Existence then
         Raise_Missing_Return_Value_For_Mocked_Function_Exception
           (Function_Name => "Dump",
            Package_Name  => "Reactive.Infrastructure.Mock");
      end if;

      return This.Return_Values.Dump;
   end Dump;

   not overriding
   function Get_Apply_For (This      : in Road_Sign.Mock.Object;
                           Traveller : in Agent.Agent_Id)
   return Boolean is
   begin
      return This.Applied_For.Contains (Traveller);
   end Get_Apply_For;

   procedure Set_Return_Value_For_Dump (
      This         : in out Road_Sign.Mock.Object;
      Return_Value : in     G_JSON.JSON_Value) is
   begin
      This.Return_Values.Dump := Return_Value;
      This.Return_Values.Dump_Existence := TRUE;
   end Set_Return_Value_For_Dump;

end Passive.Road_Sign.Mock;
