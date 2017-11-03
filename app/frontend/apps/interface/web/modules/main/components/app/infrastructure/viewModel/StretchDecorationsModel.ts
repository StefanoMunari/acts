import {BusModel} from "./bus.model";

export class StretchDecorationsModel{
    constructor(public pedestrianCrossing: boolean = false,
                public bicycleCrossing: boolean = false,
                public busStop: Array<BusModel>,
                public facilityId: number = null) {}
}