import {LaneModel} from "./lane.model";

export class WayModel {
    constructor(public id: number,
                public ordinal: number,
                public lanes: Array<LaneModel>) {}
}