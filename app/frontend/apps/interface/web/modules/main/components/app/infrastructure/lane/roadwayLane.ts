import {LaneModel} from "../viewModel/lane.model";
import {Lane} from "./lane";
import {RoadwayStretch} from "../stretch/roadwayStretch";
import {StraightDirection} from "../../shared/straightDirection";
import {DirectionUtils} from "../../shared/directionUtils";
import {StretchDecorationsModel} from "../viewModel/StretchDecorationsModel";

export class RoadwayLane extends Lane {
    public static createFromModel(l: LaneModel) {
        let lane : RoadwayLane = new RoadwayLane();
        lane.setId(l.id);
        lane.setDirection(StraightDirection[l.direction]);
        for (let s of l.stretches) {
            let stretch : RoadwayStretch  = new RoadwayStretch();
            stretch.setId(s.id);
            stretch.setLane(lane);
            stretch.setOrientation(DirectionUtils.getOrientationByDirection(lane.getDirection()));

            let decorations: StretchDecorationsModel = s.decorations;
            if (!!decorations) {
                if (!!decorations.pedestrianCrossing) {
                    stretch.withPedestrianCrossing();
                }

                if (!!decorations.bicycleCrossing) {
                    stretch.withBikeCrossing();
                }

                if (!!decorations.busStop && decorations.busStop.length > 0) {
                    stretch.withBusStop();
                }
            }

            lane.addStretch(stretch);
        }
        return lane;
    }
}