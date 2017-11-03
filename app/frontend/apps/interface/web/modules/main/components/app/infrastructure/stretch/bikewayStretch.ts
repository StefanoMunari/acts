import {Stretch} from "./stretch";
import {Color} from "../../shared/color";
import {Pedestrian} from "../../traveller/model/pedestrian";
import {BicycleMotion} from "../../traveller/travellerMotion/bicycleMotion";

export class BikewayStretch extends Stretch {
    public getColor(): Color {
        return Color.GREEN;
    }

    public tread(traveller : Pedestrian, visitor : BicycleMotion) {
        visitor.moveToStretch(this, traveller);
    }
}