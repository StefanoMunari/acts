package body Active.Travel.Travel_Planning.Exceptions is

   procedure Raise_Isolated_Street_Exception (Street_Id : Infra_Id) is
   begin
      raise Isolated_Street
        with ("The street with id =>" & Infra_Id'Image
              (Street_Id)
              & " is not connected with any other street");
   end Raise_Isolated_Street_Exception;

   procedure Raise_Dead_Street_Exception (Street_Id : Infra_Id) is
   begin
      raise Dead_Street
        with ("The street with id =>"
              & Infra_Id'Image
                (Street_Id)
              & " has a dead way");
   end Raise_Dead_Street_Exception;

   procedure Raise_Segmented_District_Exception
     (Route_Source_Id, Route_Destination_Id : Infra_Id) is
   begin
      raise Segmented_District with "The infrastructures with id =>"
        & Infra_Id'Image (Route_Source_Id)
        & " and the one with id =>"
        & Infra_Id'Image (Route_Destination_Id)
        & " are not interconnected.";
   end Raise_Segmented_District_Exception;

   procedure Raise_Wrong_Intersection_Ways_Number_Exception
     (Intersection_Id : in Infra_Id) is
   begin
      raise Dead_Street
        with ("The intersection with id =>"
              & Infra_Id'Image (Intersection_Id)
              & " has a wrong number of ways");
   end Raise_Wrong_Intersection_Ways_Number_Exception;

   procedure Raise_Empty_SubSlice
      (SubSliceName : in String) is
   begin
      raise SubSlice
        with ("The subslice "
              & SubSliceName
              & " is empty");
   end Raise_Empty_SubSlice;

end Active.Travel.Travel_Planning.Exceptions;
