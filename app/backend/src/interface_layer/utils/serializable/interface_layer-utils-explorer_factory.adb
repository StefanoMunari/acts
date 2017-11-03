with Interface_Layer.Utils.Explorer.Pedestrian;
with Interface_Layer.Utils.Explorer.Vehicle.Bicycle;
with Interface_Layer.Utils.Explorer.Vehicle.Bus;
with Interface_Layer.Utils.Explorer.Vehicle.Private_Motor_Vehicle;
with Interface_Layer.Utils.Types.Exceptions;

package body Interface_Layer.Utils.Explorer_Factory is

   function Get_Explorer (Traveller_Type : Types.Data_Type)
   return Explorer.Reference is
   begin
      case Traveller_Type is

         when Types.PEDESTRIAN            =>
            return new Explorer.Pedestrian.Object;

         when Types.BICYCLE               =>
            return new Explorer.Vehicle.Bicycle.Object;

         when Types.BUS                   =>
            return new Explorer.Vehicle.Bus.Object;

         when Types.PRIVATE_MOTOR_VEHICLE =>
            return new Explorer.Vehicle.Private_Motor_Vehicle.Object;

         when others                      =>
            raise Types.Exceptions.Wrong_Type
            with "Explorer_Factory::Get_Explorer - "
                 & Types.Data_Type'Image (Traveller_Type);

      end case;
   end Get_Explorer;

end Interface_Layer.Utils.Explorer_Factory;
