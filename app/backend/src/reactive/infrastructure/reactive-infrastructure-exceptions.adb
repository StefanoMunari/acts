package body Reactive.Infrastructure.Exceptions is

   procedure Raise_Street_Without_Lanes_Exception (
      Street_Id          : in Infra_Id;
      Incoming_Direction : in Direction.Straight) is
   begin
      raise Street_Without_Lanes
        with "The Street with id =>"
        & Infra_Id'Image (Street_Id)
        & " has no lanes for travel direction "
        & Direction.Straight'Image (Incoming_Direction);
   end Raise_Street_Without_Lanes_Exception;

   procedure Raise_Few_Streets_Connected_With_Exception (
      Intersection_Id       : in Infra_Id;
      Streets_Count         : in Natural;
      Intersection_Type     : in Intersection.Intersection_Type;
      Intersection_Ways_Num : in Natural) is
   begin
      raise Few_Streets_Connected_With
        with "There "
        & (if Streets_Count = 1 then "is" else "are")
        & " only"
        & Natural'Image (Streets_Count)
        & " street"
        & (if Streets_Count /= 1 then "s" else "")
        & " connected with the "
        & Intersection.Intersection_Type'Image (Intersection_Type)
        & " with id =>"
        & Infra_Id'Image (Intersection_Id)
        & "! The right number of streets to connect to it is"
        & Natural'Image (Intersection_Ways_Num);
   end Raise_Few_Streets_Connected_With_Exception;

   procedure Raise_Too_Many_Streets_Connected_With_Exception (
      Intersection_Id       : in Infra_Id;
      Streets_Count         : in Natural;
      Intersection_Type     : in Intersection.Intersection_Type;
      Intersection_Ways_Num : in Natural) is
   begin
      raise Too_Many_Streets_Connected_With
        with "There are"
        & Natural'Image (Streets_Count)
        & " streets connected with the "
        & Intersection.Intersection_Type'Image (Intersection_Type)
        & " with id =>"
        & Infra_Id'Image (Intersection_Id)
        & "! The right number of streets to connect to it is"
        & Natural'Image (Intersection_Ways_Num);
   end Raise_Too_Many_Streets_Connected_With_Exception;

   procedure Raise_Intersection_With_Undefined_Id_Exception is
   begin
      raise Intersection_With_Undefined_Id
        with "The intersection id is undefined";
   end Raise_Intersection_With_Undefined_Id_Exception;

   procedure Raise_Wrong_Travel_Direction_Exception (
      Lane_Id               : in Infra_Id;
      Lane_Travel_Direction : in Direction.Straight;
      Street_Id             : in Infra_Id;
      Street_Orientation    : in Direction.Orientation) is
   begin
      raise Wrong_Travel_Direction
        with "The travel direction "
        & Direction.Straight'Image (Lane_Travel_Direction)
        & " of lane with id =>"
        & Infra_Id'Image (Lane_Id)
        & " is not compatible with "
        & Direction.Orientation'Image (Street_Orientation)
        & " orientation street (id =>"
        & Infra_Id'Image (Street_Id)
        & ")";
   end Raise_Wrong_Travel_Direction_Exception;

   procedure
   Raise_Exceded_Requests_For_Connection_With_A_Single_Street_Exception (
      Street_Id : in Infra_Id) is
   begin
      raise Exceded_Requests_For_Connection_With_A_Single_Street
        with "The Street with id =>"
        & Infra_Id'Image (Street_Id)
        & " had been already reached the maximum number of connections"
        & " with intersection";
   end Raise_Exceded_Requests_For_Connection_With_A_Single_Street_Exception;

   procedure Raise_Intersection_Connection_Already_Present_Exception (
      Intersection_Id : in Infra_Id;
      Direction       : in Shared.Direction.Cardinal) is
   begin
      raise Intersection_Connection_Already_Present
        with "The intersection with id =>"
        & Infra_Id'Image (Intersection_Id)
        & " is already connected with a street at "
        & Shared.Direction.Cardinal'Image (Direction);
   end Raise_Intersection_Connection_Already_Present_Exception;

   procedure Raise_Wrong_Direction_Street_Connection_Exception (
      Street_Id : in Infra_Id;
      Direction : in Shared.Direction.Cardinal) is
   begin
      raise Wrong_Direction_Street_Connection
        with "The Street with id =>"
        & Infra_Id'Image (Street_Id)
        & " is already connected with a intersection at "
        & Shared.Direction.Cardinal'Image (Direction)
        & " so it will have to be connected only at "
        & Shared.Direction.Cardinal'Image
        (Shared.Direction.Get_Inverse_Direction(Direction))
        & " of another intersection ";
   end Raise_Wrong_Direction_Street_Connection_Exception;

   procedure Raise_Street_Orientation_Incompatible_With_Intersection_Exit_Exception (
      Street_Id, Intersection_Id : in Infra_Id;
      Street_Orientation         : in Direction.Orientation;
      Intersection_Direction     : in Direction.Cardinal) is
   begin
      raise Street_Orientation_Incompatible_With_Intersection_Exit
        with "The Street with id =>"
        & Infra_Id'Image (Street_Id)
        & " and orientation => "
        & Direction.Orientation'Image (Street_Orientation)
        & " is requested to be connected at "
        & Shared.Direction.Cardinal'Image (Intersection_Direction)
        & " of the intersection with id =>"
        & Infra_Id'Image (Intersection_Id);
   end Raise_Street_Orientation_Incompatible_With_Intersection_Exit_Exception;

   procedure Raise_Stretch_Missing_Into_Lane_Exception (
      Stretch_Id : in Infra_Id;
      Lane_Id    : in Infra_Id) is
   begin
      raise Stretch_Missing_Into_Lane
        with "Stretch with id =>"
        & Infra_Id'Image (Stretch_Id)
        & " missing into the related Lane_Id: the lane with id =>"
        & Infra_Id'Image (Lane_Id);
   end Raise_Stretch_Missing_Into_Lane_Exception;

   procedure Raise_No_Stretches_In_Lane_Exception (Lane_Id : in Infra_Id) is
   begin
      raise No_Stretches_In_Lane
        with "The Lane with id =>"
        & Infra_Id'Image (Lane_Id)
        & " has no stretches";
   end Raise_No_Stretches_In_Lane_Exception;

   procedure Raise_Missing_Stretch_In_Lane_Exception (
      Stretch_Id, Lane_Id : in Infra_Id) is
   begin
      raise Missing_Stretch_In_Lane
        with "The Lane with id =>"
        & Infra_Id'Image (Lane_Id)
        & " does not contain the stretch with id =>"
        & Infra_Id'Image (Stretch_Id);
   end Raise_Missing_Stretch_In_Lane_Exception;

   procedure Raise_Missing_Lane_Comply_With_Traveller_Direction_Exception (
      Traveller_Id        : in Agent.Agent_Id;
      Traveller_Direction : in Direction.Straight;
      Way_Id              : in Infra_Id) is
   begin
      raise Missing_Lane_Comply_With_Traveller_Direction
        with "The way with id =>"
        & Infra_Id'Image (Way_Id)
        --& " has no lanes compatible with traveller (id =>"
        --& Natural'Image (Traveller_Id)
        & ") direction =>"
        & Direction.Straight'Image (Traveller_Direction);
   end Raise_Missing_Lane_Comply_With_Traveller_Direction_Exception;

end Reactive.Infrastructure.Exceptions;
