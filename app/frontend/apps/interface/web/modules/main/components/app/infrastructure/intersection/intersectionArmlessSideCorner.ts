import {Street} from "../street/street";
import {CardinalDirection} from "../../shared/cardinalDirection";
import {Way} from "../way/way";
import {IntersectionArmlessSide} from "./intersectionArmlessSide";
import {Intersection} from "./intersection";
import {IntersectionArmlessSideWay} from "./intersectionArmlessSideWay";
import {IntersectionArmlessSideWayCorner} from "./intersectionArmlessSideWayCorner";

export class IntersectionArmlessSideCorner extends IntersectionArmlessSide {

    constructor(armlessDirection: CardinalDirection,
                intersection: Intersection) {
        super(armlessDirection, intersection);
    }

    public generateArmlessSideWay(way: Way, street: Street, offset: number,
                                  referenceDirection: CardinalDirection): IntersectionArmlessSideWay {

        return new IntersectionArmlessSideWayCorner(way, street, offset,
                                                    this.getArmlessDirection(),
                                                    referenceDirection,
                                                    this.getIntersection())
    }

}