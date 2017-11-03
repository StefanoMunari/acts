with Active.Traveller.Pedestrian.Extractor;
with Active.Traveller.Vehicle.Bicycle.Extractor;
with Active.Traveller.Vehicle.Bus.Extractor;
with Active.Traveller.Vehicle.Private_Motor_Vehicle.Extractor;

with Interface_Layer.Utils.Types.Exceptions;

package body Active.Traveller.Extractor_Factory is

   package PVM_Pkg renames Active.Traveller.Vehicle.Private_Motor_Vehicle;

   function Get_Extractor (Traveller_Type : Types.Data_Type)
   return Shared.Extractable.Reference is
   begin
      case Traveller_Type is
         when Types.PEDESTRIAN            => return
            new Active.Traveller.Pedestrian.Extractor.Object;
         when Types.BICYCLE               => return
            new Active.Traveller.Vehicle.Bicycle.Extractor.Object;
         when Types.BUS                   => return
            new Active.Traveller.Vehicle.Bus.Extractor.Object;
         when Types.PRIVATE_MOTOR_VEHICLE => return
            new PVM_Pkg.Extractor.Object;
         when others                      =>
            raise Types.Exceptions.Wrong_Type
               with "Extractor_Factory::Get_Extractor - "
                  & Types.Data_Type'Image (Traveller_Type);
      end case;
   end Get_Extractor;

end Active.Traveller.Extractor_Factory;
