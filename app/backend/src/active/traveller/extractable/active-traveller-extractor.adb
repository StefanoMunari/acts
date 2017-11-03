with Ada.Unchecked_Deallocation;

with Active.Travel;

with Shared.Infra_Id_List;
with Shared.Infra_Id_List_Utils; use Shared.Infra_Id_List_Utils;
with Shared.Slice;

package body Active.Traveller.Extractor is

   package Slice               renames Shared.Slice;

   procedure Extract (This      : in     Extractor.Object;
                      Traveller : in     Traveller_Pkg.Object'Class;
                      Result    : in out Explorer.Object'Class)
   is
      Route       : Infra_Id_List.List;
      Source      : Slice.Map;
      Destination : Slice.Map;
   begin
      Route       := Traveller.Travel_Ref.Get_Residual_Route;
      Source      := Traveller.Get_Travel_Source;
      Destination := Traveller.Get_Travel_Destination;
      Explorer.Init (
         Result,
         Traveller.Id,
         Traveller.Maximum_Speed,
         Traveller.Current_Speed,
         Traveller.Current_Position,
         Route,
         Source,
         Destination);
   end Extract;

   procedure Extract (This     : in     Extractor.Object;
                      Instance : in     Explorer.Object'Class;
                      Result   : in out Traveller_Pkg.Object'Class)
   is
   begin
      Result.Current_Speed := Explorer.Object (Instance).Current_Speed;
      Result.Current_Position := Instance.Position;
   end Extract;

end Active.Traveller.Extractor;
