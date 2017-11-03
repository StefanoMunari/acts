import {StreetModel} from "./street.model";
import {IntersectionModel} from "./intersection.model";
import {FacilityModel} from "../facility/viewModel/facility.model";

export class DistrictModel {
    streets: Array<StreetModel>;
    intersections: Array<IntersectionModel>;
    facilities: Array<FacilityModel>;
    neighbors: Array<string>;
}