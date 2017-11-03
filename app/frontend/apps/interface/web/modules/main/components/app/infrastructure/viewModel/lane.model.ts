import {StretchModel} from "./stretch.model";

export class LaneModel {
    constructor(public id: number,
                public direction: string,
                public stretches: Array<StretchModel>) {}
}