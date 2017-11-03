import {Lane} from "./lane";
import {ResidentialStretch} from "../stretch/residentialStretch";
import {StraightDirection} from "../../shared/straightDirection";
import {DirectionUtils} from "../../shared/directionUtils";

export class ResidentialLane extends Lane {
    public static create(direction: StraightDirection, length: number) {
        let lane: ResidentialLane = new ResidentialLane();
        lane.setDirection(direction);
        for (let i = 0; i < length; ++i) {
            let stretch : ResidentialStretch = new ResidentialStretch();
            stretch.setLane(lane);
            stretch.setOrientation(DirectionUtils.getOrientationByDirection(direction));
            lane.addStretch(stretch);
        }
        return lane;
    }

    public putStretchAt(index: number, stretch: ResidentialStretch) {
        stretch.setLane(this);
        this.getStretches()[index] = stretch;
    }

}