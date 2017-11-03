with Reactive;

package Interface_Layer.Utils.Types is

   use Reactive.Infra_Id_Type;

   type Request_Type is
     (ENTER, TREAD, SNAPSHOT, QUERY, BOOT, SHUTDOWN, ENTER_BUILDING,
      EXIT_BUILDING, TRAFFIC_LIGHT, ENTER_BUS, EXIT_BUS);

   type Data_Type is
     (ACK, MESSAGE, PEDESTRIAN, BICYCLE, BUS, PRIVATE_MOTOR_VEHICLE);

   type Call_Type is
     (SYNC, ASYNC);

   type Recipient_Sort is
     (TREADABLE, HOST);

   package Recipient_Type_Pkg is
      type Recipient_Type is record
         Id   : Infra_Id;
         Sort : Types.Recipient_Sort;
      end record;
   end Recipient_Type_Pkg;

end Interface_Layer.Utils.Types;