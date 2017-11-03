import {WayModel} from "./way.model";

export class StreetModel {
    constructor (public id: number,
                 public orientation: string,
                 public roadways: Array<WayModel>,
                 public bikeways: Array<WayModel>,
                 public footways: Array<WayModel>) {}
}