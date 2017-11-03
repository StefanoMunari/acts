import {ExitStretch} from "./exitStretch";
import {CardinalDirection} from "../../shared/cardinalDirection";
import {Street} from "../street/street";
import {Intersection} from "./intersection";
import {Way} from "../way/way";
import {DrawerAdapter} from "../../draw/drawerAdapter";
import {ShapeComponent} from "../../draw/shape.component";
import {Infrastructure} from "../infrastructure";

export class ExitRoadwayStretch implements Infrastructure {

    constructor(private exitDirection: CardinalDirection,
                private intersection: Intersection) {}

    public getShapes(minX: number, minY: number,
                     maxXPlusWidth: number, maxYPlusHeight: number,
                     drawer: DrawerAdapter): Array<ShapeComponent> {

        let street: Street = this.intersection.getStreets().get(this.exitDirection);
        if (street !== null) {
            let roadWay: Way = street.getRoadway();
            let offset: number = 0;
            return new ExitStretch(this.exitDirection, roadWay, street, offset, this.intersection)
                .getShapes(minX, minY, maxXPlusWidth, maxYPlusHeight, drawer);
        }
        return null;
    }
}