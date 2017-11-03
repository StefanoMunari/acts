with Active.Agent;

with Reactive.Infrastructure.Intersection;

with Shared.Direction;

package Reactive.Infrastructure.Exceptions is

   package Agent renames Active.Agent;
   package Direction renames Shared.Direction;
   use Reactive.Infra_Id_Type;

   Street_Without_Lanes : exception;
   Few_Streets_Connected_With : exception;
   Too_Many_Streets_Connected_With : exception;
   Intersection_With_Undefined_Id : exception;
   Exceded_Requests_For_Connection_With_A_Single_Street : exception;
   Intersection_Connection_Already_Present : exception;
   Wrong_Direction_Street_Connection : exception;
   Street_Orientation_Incompatible_With_Intersection_Exit : exception;
   Wrong_Travel_Direction : exception;
   Stretch_Missing_Into_Lane : exception;
   No_Stretches_In_Lane : exception;
   Missing_Stretch_In_Lane : exception;
   Missing_Lane_Comply_With_Traveller_Direction : exception;

   procedure Raise_Street_Without_Lanes_Exception (
      Street_Id          : in Infra_Id;
      Incoming_Direction : in Direction.Straight);

   procedure Raise_Few_Streets_Connected_With_Exception (
      Intersection_Id       : in Infra_Id;
      Streets_Count         : in Natural;
      Intersection_Type     : in Intersection.Intersection_Type;
      Intersection_Ways_Num : in Natural);

   procedure Raise_Too_Many_Streets_Connected_With_Exception (
      Intersection_Id       : in Infra_Id;
      Streets_Count         : in Natural;
      Intersection_Type     : in Intersection.Intersection_Type;
      Intersection_Ways_Num : in Natural);

   procedure Raise_Intersection_With_Undefined_Id_Exception;

   procedure
   Raise_Exceded_Requests_For_Connection_With_A_Single_Street_Exception (
      Street_Id : in Infra_Id);

   procedure Raise_Intersection_Connection_Already_Present_Exception (
      Intersection_Id : in Infra_Id;
      Direction       : in Shared.Direction.Cardinal);

   procedure Raise_Wrong_Direction_Street_Connection_Exception (
      Street_Id : in Infra_Id;
      Direction : in Shared.Direction.Cardinal);

   procedure Raise_Street_Orientation_Incompatible_With_Intersection_Exit_Exception (
      Street_Id, Intersection_Id : in Infra_Id;
      Street_Orientation         : in Direction.Orientation;
      Intersection_Direction     : in Direction.Cardinal);

   procedure Raise_Wrong_Travel_Direction_Exception (
      Lane_Id               : in Infra_Id;
      Lane_Travel_Direction : in Direction.Straight;
      Street_Id             : in Infra_Id;
      Street_Orientation    : in Direction.Orientation);

   procedure Raise_Stretch_Missing_Into_Lane_Exception (
      Stretch_Id : in Infra_Id;
      Lane_Id    : in Infra_Id);

   procedure Raise_No_Stretches_In_Lane_Exception (Lane_Id : in Infra_Id);

   procedure Raise_Missing_Stretch_In_Lane_Exception (
      Stretch_Id, Lane_Id : in Infra_Id);

   procedure Raise_Missing_Lane_Comply_With_Traveller_Direction_Exception (
      Traveller_Id        : in Agent.Agent_Id;
      Traveller_Direction : in Direction.Straight;
      Way_Id              : in Infra_Id);

end Reactive.Infrastructure.Exceptions;
