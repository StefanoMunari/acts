import {DistrictSummaryModel} from "./district-summary.model";

export class CityModel {
    constructor(
      public id        : string,
      public name      : string,
      public color     : string,
      public lColor    : string,
      public districts : Array<DistrictSummaryModel>
    ) {}
}