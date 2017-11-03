import {Street} from "../street/street";
import {CardinalDirection} from "../../shared/cardinalDirection";
import {Intersection} from "./intersection";
import {IntersectionUtils} from "./intersection.utils";
import {Way} from "../way/way";
import {Infrastructure} from "../infrastructure";
import {DrawerAdapter} from "../../draw/drawerAdapter";
import {ShapeComponent} from "../../draw/shape.component";
import {IntersectionArmlessSideWay} from "./intersectionArmlessSideWay";

export class IntersectionArmlessSide implements Infrastructure {
    constructor(private armlessDirection: CardinalDirection,
                private intersection: Intersection) {}

    public getArmlessDirection(): CardinalDirection {
        return this.armlessDirection;
    }

    public getIntersection(): Intersection {
        return this.intersection;
    }

    public getShapes(minX: number, minY: number,
                     maxXPlusWidth: number,
                     maxYPlusHeight: number,
                     drawer: DrawerAdapter): ShapeComponent[] {

        let shapes: Array<ShapeComponent> = [];

        let referenceDirection: CardinalDirection =
            IntersectionUtils.findReferenceDirection(this.intersection,
                                                     this.armlessDirection);
        let street: Street = this.intersection.getStreets().get(referenceDirection);

        let offset: number = 0;

        if (this.armlessDirection === CardinalDirection.EAST) {
            offset = this.intersection.getWidth();
        } else if (this.armlessDirection === CardinalDirection.SOUTH) {
            offset = this.intersection.getHeight();
        }

        for (let way of street.getWaysFirstRoadway()) {

            if (this.armlessDirection === CardinalDirection.WEST ||
                this.armlessDirection === CardinalDirection.NORTH) {
                offset = offset + way.getWidth();
            }

            let armlessSideWay: IntersectionArmlessSideWay =
                this.generateArmlessSideWay(way, street, offset, referenceDirection);
            shapes = shapes.concat(armlessSideWay.getShapes(minX, minY, maxXPlusWidth, maxYPlusHeight, drawer));

            if (this.armlessDirection === CardinalDirection.EAST ||
                this.armlessDirection === CardinalDirection.SOUTH) {
                offset = offset + way.getWidth();
            }
        }

        return shapes;
    }

    public generateArmlessSideWay(way: Way, street: Street, offset: number,
                                  referenceDirection: CardinalDirection): IntersectionArmlessSideWay {
        return new IntersectionArmlessSideWay(way, street, offset,
                                              this.armlessDirection,
                                              referenceDirection,
                                              this.intersection);
    }

}