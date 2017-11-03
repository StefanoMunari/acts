package Reactive.Infrastructure_Registry.Exceptions is
   use Reactive.Infra_Id_Type;

   Infrastructure_Missing : exception;
   Intersection_Missing : exception;
   Street_Missing : exception;
   Street_Related_Infrastructure_Missing : exception;
   Way_Missing : exception;
   Roadway_Missing : exception;
   Footway_Missing : exception;
   Bikeway_Missing : exception;
   Lane_Missing : exception;
   Stretch_Missing : exception;
   Traveller_Missing : exception;
   Infrastructure_Already_Existent : exception;

   procedure Raise_Infrastructure_Missing_Exception (
      Infrastructure_Id : in Infra_Id);

   procedure Raise_Intersection_Missing_Exception (
      Intersection_Id : in Infra_Id);

   procedure Raise_Street_Missing_Exception (Street_Id : in Infra_Id);

   procedure Raise_Street_Related_Infrastructure_Missing_Exception
     (Street_Related_Infrastructure_Id : in Infra_Id);

   procedure Raise_Way_Missing_Exception (Way_Id : in Infra_Id);

   procedure Raise_Roadway_Missing_Exception (Roadway_Id : in Infra_Id);

   procedure Raise_Footway_Missing_Exception (Footway_Id : in Infra_Id);

   procedure Raise_Bikeway_Missing_Exception (Bikeway_Id : in Infra_Id);

   procedure Raise_Lane_Missing_Exception (Lane_Id : in Infra_Id);

   procedure Raise_Stretch_Missing_Exception (Stretch_Id : in Infra_Id);

   procedure Raise_Traveller_Missing_Exception (Traveller_Id : in Infra_Id);

   procedure Raise_Infrastructure_Already_Existent_Exception
     (Infrastructure_Id : Infra_Id);

end Reactive.Infrastructure_Registry.Exceptions;
