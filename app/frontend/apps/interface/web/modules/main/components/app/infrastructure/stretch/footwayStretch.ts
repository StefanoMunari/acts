import {Stretch} from "./stretch";
import {Color} from "../../shared/color";
import {Pedestrian} from "../../traveller/model/pedestrian";
import {PedestrianMotion} from "../../traveller/travellerMotion/pedestrianMotion";

export class FootwayStretch extends Stretch {
    public getColor(): Color {
        return Color.GRAY;
    }

    public tread(traveller : Pedestrian, visitor : PedestrianMotion) {
        visitor.moveToStretch(this, traveller);
    }
}