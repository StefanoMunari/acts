with Active.Traveller.Utils;

with Interface_Layer.Utils.Explorer.Vehicle;

with Shared.Agent_Id_List;

package body Active.Traveller.Vehicle.Extractor is

   package Agent_Id_List renames Shared.Agent_Id_List;

   procedure Extract (
      This     : in     Extractor.Object;
      Instance : in     Traveller_Pkg.Object'Class;
      Result   : in out Explorer.Object'Class)
   is
      Max_Passengers : Natural;
      Passengers     : Agent_Id_List.List;
   begin
   -- UPCAST
   --   This : Vehicle.Extractor => This : Traveller.Extractor
   --   To obtain an Explorer (Result) filled with data
   -- NOTE
   --   Result should have dynamic_type (Explorer.Vehicle)
   --   Result inherits the record from Explorer
      Traveller_Pkg.Extractor.Extract(
         Traveller_Pkg.Extractor.Object (This), Instance, Result);
      Max_Passengers := Vehicle_Pkg.Object'Class (Instance).Max_Passengers;
      Passengers     := Vehicle_Pkg.Object'Class (Instance).Passengers;
   -- Complete Vehicle's Initialization
   -- NOTE
   --   Since Vehicle.Extractor is a child package of
   --   Active.Traveller.Vehicle, it can access private fields of
   --   Vehicle.Extractor
      Explorer_Vehicle.Init (
         Explorer_Vehicle.Object (Result), Max_Passengers, Passengers);
    end Extract;


   procedure Extract (
      This     : in     Extractor.Object;
      Instance : in     Explorer.Object'Class;
      Result   : in out Traveller_Pkg.Object'Class)
    is
    begin
   -- UPCAST
   --   This : Vehicle.Extractor => This : Traveller.Extractor
   --   To obtain a Traveller (Result) filled with data
   -- NOTE
   --   Result should have dynamic_type (Traveller.Vehicle)
   --   Result inherits the record from Traveller
      Traveller_Pkg.Extractor.Object (This).Extract (Instance, Result);
      Vehicle_Pkg.Object (Result).Passengers :=
         Explorer.Vehicle.Object (Instance).Passengers;
    end Extract;

end Active.Traveller.Vehicle.Extractor;
