import {IntersectionExitModel} from "./intersectionExit.model";

export class IntersectionModel {
    constructor (public id: number,
                 public exits: Array<IntersectionExitModel>) {}
}