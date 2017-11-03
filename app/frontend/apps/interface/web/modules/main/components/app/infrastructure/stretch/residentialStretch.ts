import {Stretch} from "./stretch";
import {Color} from "../../shared/color";
import {Vehicle} from "../../traveller/model/vehicle";
import {VehicleMotion} from "../../traveller/travellerMotion/vehicleMotion";

export class ResidentialStretch extends Stretch {
    public getColor(): Color {
        return Color.WHITE;
    }

    public tread(traveller : Vehicle, visitor : VehicleMotion) {
        visitor.moveToStretch(this, traveller);
    }
}