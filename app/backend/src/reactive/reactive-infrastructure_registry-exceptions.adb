package body Reactive.Infrastructure_Registry.Exceptions is

   procedure Raise_Infrastructure_Missing_Exception (
      Infrastructure_Id : in Infra_Id) is
   begin
      raise Infrastructure_Missing
        with "The infrastructure with id =>"
        & Infra_Id'Image(Infrastructure_Id)
        & " is not present into the entity registry ";
   end Raise_Infrastructure_Missing_Exception;

   procedure Raise_Intersection_Missing_Exception (
      Intersection_Id : in Infra_Id) is
   begin
      raise Intersection_Missing
        with "The intersection with id =>"
        & Infra_Id'Image(Intersection_Id)
        & " is not present into the entity registry ";
   end Raise_Intersection_Missing_Exception;

   procedure Raise_Street_Missing_Exception (Street_Id : in Infra_Id) is
   begin
      raise Street_Missing
        with "The Street with id =>"
        & Infra_Id'Image(Street_Id)
        & " is not present into the entity registry ";
   end Raise_Street_Missing_Exception;

   procedure Raise_Street_Related_Infrastructure_Missing_Exception
     (Street_Related_Infrastructure_Id : in Infra_Id) is
   begin
      raise Street_Related_Infrastructure_Missing
        with "The Street_Related_Infrastructure with id =>"
        & Infra_Id'Image(Street_Related_Infrastructure_Id)
        & " is not present into the entity registry ";
   end Raise_Street_Related_Infrastructure_Missing_Exception;

   procedure Raise_Way_Missing_Exception (Way_Id : in Infra_Id) is
   begin
      raise Way_Missing
        with "The Way with id =>"
        & Infra_Id'Image(Way_Id)
        & " is not present into the entity registry ";
   end Raise_Way_Missing_Exception;

   procedure Raise_Roadway_Missing_Exception (Roadway_Id : in Infra_Id) is
   begin
      raise Roadway_Missing
        with "The Roadway with id =>"
        & Infra_Id'Image(Roadway_Id)
        & " is not present into the entity registry ";
   end Raise_Roadway_Missing_Exception;

   procedure Raise_Footway_Missing_Exception (Footway_Id : in Infra_Id) is
   begin
      raise Footway_Missing
        with "The Footway with id =>"
        & Infra_Id'Image(Footway_Id)
        & " is not present into the entity registry ";
   end Raise_Footway_Missing_Exception;

   procedure Raise_Bikeway_Missing_Exception (Bikeway_Id : in Infra_Id) is
   begin
      raise Bikeway_Missing
        with "The Bikeway with id =>"
        & Infra_Id'Image(Bikeway_Id)
        & " is not present into the entity registry ";
   end Raise_Bikeway_Missing_Exception;

   procedure Raise_Lane_Missing_Exception (Lane_Id : in Infra_Id) is
   begin
      raise Lane_Missing
        with "The Lane with id =>"
        & Infra_Id'Image(Lane_Id)
        & " is not present into the entity registry ";
   end Raise_Lane_Missing_Exception;

   procedure Raise_Stretch_Missing_Exception (Stretch_Id : in Infra_Id) is
   begin
      raise Stretch_Missing
        with "The Stretch with id =>"
        & Infra_Id'Image(Stretch_Id)
        & " is not present into the entity registry ";
   end Raise_Stretch_Missing_Exception;

   procedure Raise_Traveller_Missing_Exception (Traveller_Id : in Infra_Id) is
   begin
      raise Traveller_Missing
        with "The Traveller with id =>"
        & Infra_Id'Image(Traveller_Id)
        & " is not present into the entity registry ";
   end Raise_Traveller_Missing_Exception;

   procedure Raise_Infrastructure_Already_Existent_Exception
     (Infrastructure_Id : Infra_Id) is
   begin
      raise Infrastructure_Already_Existent
        with "The infrastructure with id =>"
        & Infra_Id'Image (Infrastructure_Id)
        & " is already present into entity registry";
   end Raise_Infrastructure_Already_Existent_Exception;

end Reactive.Infrastructure_Registry.Exceptions;
