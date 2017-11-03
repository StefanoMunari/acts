import {CardinalDirection} from "../../shared/cardinalDirection";
import {Street} from "../street/street";
import {Way} from "../way/way";
import {ExitStretch} from "./exitStretch";
import {Intersection} from "./intersection";
import {ExitStretch2} from "./exitStretch2";
import {ExitStreet} from "./exitStreet";

export class ExitStreet2 extends ExitStreet {

    constructor(exitDirection: CardinalDirection,
                ways: Array<Way>, street: Street,
                intersection: Intersection) {
        super(exitDirection, ways, street, intersection);
    }

    public generateExitStretch(way: Way, offset: number): ExitStretch {
        return new ExitStretch2(this.getExitDirection(), way, this.getStreet(),
                                offset, this.getIntersection());
    }

}