import {Traveller} from "../traveller/model/traveller";
import {TravellerMotion} from "../traveller/travellerMotion/travellerMotion";

export interface Treadable {
    tread(traveller: Traveller, visitor: TravellerMotion);
    getId(): number;
}