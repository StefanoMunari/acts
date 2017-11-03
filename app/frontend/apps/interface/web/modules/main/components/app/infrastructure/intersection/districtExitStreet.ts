import {CardinalDirection} from "../../shared/cardinalDirection";
import {Street} from "../street/street";
import {Way} from "../way/way";
import {ExitStretch} from "./exitStretch";
import {Intersection} from "./intersection";
import {ExitStreet} from "./exitStreet";
import {DistrictExitStretch} from "./districtExitStretch";

export class DistrictExitStreet extends ExitStreet {

    constructor(exitDirection: CardinalDirection,
                ways: Array<Way>, street: Street,
                intersection: Intersection) {
        super(exitDirection, ways, street, intersection);
    }

    public generateExitStretch(way: Way, offset: number): ExitStretch {
        return new DistrictExitStretch(this.getExitDirection(), way,
                                       this.getStreet(), offset,
                                       this.getIntersection());
    }

}