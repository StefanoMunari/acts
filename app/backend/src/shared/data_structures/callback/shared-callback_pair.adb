-- core
with Ada.Unchecked_Deallocation;

with Shared.Callback_Pair.Exceptions;

package body Shared.Callback_Pair is

   package Callback_Exc_Pkg renames Shared.Callback_Pair.Exceptions;

   function Create (
      Success : Success_Pkg.Reference;
      Failure : Failure_Pkg.Reference)
   return Callback_Pair.Object is
      This : Callback_Pair.Object;
   begin
      This.Success := Success;
      This.Failure := Failure;
      return This;
   end Create;

   procedure Initialize (This : in out Callback_Pair.Object)
   is begin
      null;
   end Initialize;

   procedure Adjust (This : in out Callback_Pair.Object)
   is begin
      null;
   end Adjust;

   procedure Finalize (This : in out Callback_Pair.Object)
   is
   begin
      null;
   end Finalize;

end Shared.Callback_Pair;
