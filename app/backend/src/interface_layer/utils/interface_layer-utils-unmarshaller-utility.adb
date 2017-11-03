with Ada.Text_IO;

with Interface_Layer.Utils.Explorer;
with Interface_Layer.Utils.Explorer.Vehicle;
with Interface_Layer.Utils.Explorer.Vehicle.Private_Motor_Vehicle;
with Interface_Layer.Utils.Explorer.Vehicle.Bus;
with Interface_Layer.Utils.Explorer.Vehicle.Bicycle;
with Interface_Layer.Utils.Explorer.Pedestrian;
with Interface_Layer.Utils.Types;
with Interface_Layer.Utils.Types.Exceptions;
with Interface_Layer.Utils.Unmarshaller.Message;
with Interface_Layer.Utils.Unmarshaller.Ack;

package body Interface_Layer.Utils.Unmarshaller.Utility is
   use Interface_Layer.Utils.Types;

   package Explorer renames Interface_Layer.Utils.Explorer;

   function Get_Unmarshaller (Data : String)
   return Unmarshaller.Reference
   is
   begin
      case Types.Data_Type'Value (Data) is
         when Types.ACK                      =>
            return new Unmarshaller.Ack.Object;
         when Types.MESSAGE                  =>
            return new Unmarshaller.Message.Object;
         when Types.PEDESTRIAN               =>
            return new Explorer.Pedestrian.Object;
         when Types.BICYCLE                  =>
            declare
               Result : Explorer.Vehicle.Bicycle.Reference
                  := new Explorer.Vehicle.Bicycle.Object;
            begin
               Result.Set_District;
               return Result;
            end;
         when Types.BUS                      =>
            declare
               Result : Explorer.Vehicle.Bus.Reference
                  := new Explorer.Vehicle.Bus.Object;
            begin
               Result.Set_District;
               return Result;
            end;
         when Types.PRIVATE_MOTOR_VEHICLE    =>
            declare
               Result : Explorer.Vehicle.Private_Motor_Vehicle.Reference
                  := new Explorer.Vehicle.Private_Motor_Vehicle.Object;
            begin
               Result.Set_District;
               return Result;
            end;
         when others                         =>
            raise Types.Exceptions.Null_Type
               with "Unmarshaller.Utility::Get_Unmarshaller";
      end case;
   end Get_Unmarshaller;

end Interface_Layer.Utils.Unmarshaller.Utility;