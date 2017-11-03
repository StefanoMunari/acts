import {LaneModel} from "../viewModel/lane.model";
import {Lane} from "./lane";
import {FootwayStretch} from "../stretch/footwayStretch";
import {StraightDirection} from "../../shared/straightDirection";
import {DirectionUtils} from "../../shared/directionUtils";

export class FootwayLane extends Lane {
    public static createFromModel(l: LaneModel) {
        let lane : FootwayLane = new FootwayLane();
        lane.setId(l.id);
        lane.setDirection(StraightDirection[l.direction]);
        for (let s of l.stretches) {
            let stretch : FootwayStretch = new FootwayStretch();
            stretch.setId(s.id);
            stretch.setLane(lane);
            stretch.setOrientation(DirectionUtils.getOrientationByDirection(lane.getDirection()));
            lane.addStretch(stretch);
        }
        return lane;
    }
}