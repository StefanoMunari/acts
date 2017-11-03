import {GarageModel} from "./garage.model";

export class FacilityModel {
    constructor(public id: number,
                public guests: Array<number>,
                public garage: GarageModel) {}
}