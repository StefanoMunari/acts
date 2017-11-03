import {LaneModel} from "../viewModel/lane.model";
import {Lane} from "./lane";
import {BikewayStretch} from "../stretch/bikewayStretch";
import {StraightDirection} from "../../shared/straightDirection";
import {DirectionUtils} from "../../shared/directionUtils";

export class BikewayLane extends Lane {
    public static createFromModel(l: LaneModel) {
        let lane : BikewayLane = new BikewayLane();
        lane.setId(l.id);
        lane.setDirection(StraightDirection[l.direction]);
        for (let s of l.stretches) {
            let stretch : BikewayStretch = new BikewayStretch();
            stretch.setId(s.id);
            stretch.setLane(lane);
            stretch.setOrientation(DirectionUtils.getOrientationByDirection(lane.getDirection()));
            lane.addStretch(stretch);
        }
        return lane;
    }
}